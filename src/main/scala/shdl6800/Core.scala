// shdl6800: A 6800 processor written in SpinalHDL
//
// Copyright (C) 2019 Oguz Meteer <info@guztech.nl>
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice appear in all copies.
//
// THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
// WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
// MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
// ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
// WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
// ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
// OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

package shdl6800

import spinal.core._

class Core extends Component {
  val io = new Bundle {
    val Addr = out Bits(16 bits)
    val Din  = in  Bits(8 bits)
    val Dout = out Bits(8 bits)
    val RW   = out Bits(1 bit)
  }

  // Registers
  val RW    = Reg(Bits(1 bit)) init(1)
  val Addr  = Reg(Bits(16 bits)) init(0)
  val Dout  = Reg(Bits(8 bits))

  val a     = Reg(Bits(8 bits))
  val b     = Reg(Bits(8 bits))
  val x     = Reg(Bits(16 bits))
  val sp    = Reg(Bits(16 bits))
  val pc    = Reg(Bits(16 bits))
  val instr = Reg(Bits(8 bits))
  val tmp8  = Reg(Bits(8 bits))
  val tmp16 = Reg(Bits(16 bits))

  // Condition Codes Register
//  val C = Reg(Bits(1 bit)) // Carry
//  val V = Reg(Bits(1 bit)) // Two's complement overflow indicator
//  val Z = Reg(Bits(1 bit)) // Zero indicator
//  val N = Reg(Bits(1 bit)) // Negative indicator
//  val I = Reg(Bits(1 bit)) // Interrupt mask
//  val H = Reg(Bits(1 bit)) // Half carry

  // Buses
  val src8_1   = Bits(8 bits)
  val src8_2   = Bits(8 bits)
  val alu8     = Bits(8 bits)
  val src16    = Bits(16 bits)
  val incdec16 = Bits(16 bits)

  // Selectors for buses
  object Reg8 extends SpinalEnum {
    val NONE, A, B, XH, XL, SPH, SPL, PCH, PCL, TMP8, TMP16H, TMP16L, DIN, DOUT = newElement()
    defaultEncoding = SpinalEnumEncoding("staticEncoding")(
      NONE   -> 0x0000,
      A      -> 0x0001,
      B      -> 0x0002,
      XH     -> 0x0004,
      XL     -> 0x0008,
      SPH    -> 0x0010,
      SPL    -> 0x0020,
      PCH    -> 0x0040,
      PCL    -> 0x0080,
      TMP8   -> 0x0100,
      TMP16H -> 0x0200,
      TMP16L -> 0x0400,
      DIN    -> 0x0800,
      DOUT   -> 0x1000
    )
  }

  object Reg16 extends SpinalEnum {
    val NONE, X, SP, PC, TMP16, ADDR = newElement()
    defaultEncoding = SpinalEnumEncoding("staticEncoding") (
      NONE  -> 0,
      X     -> 1,
      SP    -> 2,
      PC    -> 3,
      TMP16 -> 4,
      ADDR  -> 5
    )
  }

  object CCR extends SpinalEnum {
    val C, V, Z, N, I, H = newElement()
    defaultEncoding = SpinalEnumEncoding("staticEncoding")(
      C -> 1,  // Bit 0
      V -> 2,  // Bit 1
      Z -> 4,  // Bit 2
      N -> 8,  // Bit 3
      I -> 16, // Bit 4
      H -> 32  // Bit 5
    )
  }

  val src8_1_select  = Reg8()
  val src8_2_select  = Reg8()
  val alu8_write     = Bits(Reg8.elements.length bits)
  val src16_select   = Reg16()
  val src16_write    = Bits(Reg16.elements.length bits)
  val incdec16_write = Bits(Reg16.elements.length bits)
  val alu_cout       = Reg(Bits(1 bit))
  val ccr            = Reg(Bits(8 bits)) init(0) ////// REMOVE THIS LATER!!!!!!!!!!!!!!!!!!!!!
  val tmp_ccr        = Bits(8 bits)
  val ccr_mask       = Bits(8 bits)

  // Upper two bits of the CCR are always 1
  ccr(7 downto 6)     := B"11"
  tmp_ccr(7 downto 6) := B"11"
  ccr_mask            := 0

  val reg8_map = Map( Reg8.A      -> (this.a, true)
                    , Reg8.B      -> (this.b, true)
                    , Reg8.XH     -> (this.x(15 downto 8), true)
                    , Reg8.XL     -> (this.x( 7 downto 0), true)
                    , Reg8.SPH    -> (this.sp(15 downto 8), true)
                    , Reg8.SPL    -> (this.sp( 7 downto 0), true)
                    , Reg8.PCH    -> (this.pc(15 downto 8), true)
                    , Reg8.PCL    -> (this.pc( 7 downto 0), true)
                    , Reg8.TMP8   -> (this.tmp8, true)
                    , Reg8.TMP16H -> (this.tmp16(15 downto 8), true)
                    , Reg8.TMP16L -> (this.tmp16( 7 downto 0), true)
                    , Reg8.DIN    -> (this.io.Din, false)
                    , Reg8.DOUT   -> (this.Dout, true))

  val reg16_map = Map( Reg16.X     -> (this.x, true)
                     , Reg16.SP    -> (this.sp, true)
                     , Reg16.PC    -> (this.pc, true)
                     , Reg16.TMP16 -> (this.tmp16, true)
                     , Reg16.ADDR  -> (this.Addr, true))

  def src_bus_setup[T <: SpinalEnum](reg_map: Map[SpinalEnumElement[T], (Bits, Boolean)], bus: Bits, selector: SpinalEnumCraft[T]): Unit = {
    switch(selector) {
      reg_map.foreach {case (e, reg) => is(e) { bus := reg._1 }}
      default {
        bus := 0
      }
    }
  }

  def dst_bus_setup[T <: SpinalEnum](reg_map: Map[SpinalEnumElement[T], (Bits, Boolean)], bus: Bits, bitmap: Bits): Unit = {
    reg_map.foreach {case (e, reg) => if(reg._2) { when(bitmap(e.position)) {reg._1 := bus} }}
  }

  src_bus_setup(reg8_map, src8_1, src8_1_select)
  src_bus_setup(reg8_map, src8_2, src8_2_select)
  dst_bus_setup(reg8_map, alu8, alu8_write)
//  src_bus_setup(reg16_map, src16, src16_select)
//  dst_bus_setup(reg16_map, src16, src16_write)
//  dst_bus_setup(reg16_map, incdec16, incdec16_write)

  val tmp9  = Bits(9 bits)
  tmp9 := 0

  object ALU_instr extends SpinalEnum {
    val NONE, ADD, SUB, INCR, ADD_H, AND, OR, XOR, SET_ONE = newElement()
  }

  val alu_instr = ALU_instr()

  def ALU(instr: SpinalEnumCraft[ALU_instr.type]): Unit = {
    val src8_1_exp = UInt(9 bits)
    val src8_2_exp = UInt(9 bits)
    val src8_2_neg = UInt(9 bits)

    src8_1_exp := src8_1.asUInt.resize(9)
    src8_2_exp := src8_2.asUInt.resize(9)
    src8_2_neg := (~src8_2_exp)

    when(instr === ALU_instr.ADD ||
      instr === ALU_instr.SUB ||
      instr === ALU_instr.ADD_H ||
      instr === ALU_instr.INCR ||
      instr === ALU_instr.SET_ONE)
    {
      val src8_tmp_2 = UInt(9 bits)
      val carry_bit  = UInt(1 bit)

      switch(instr) {
        is(ALU_instr.ADD) {
          src8_tmp_2 := src8_2_exp
          carry_bit  := 0
        }
        is(ALU_instr.SUB) {
          src8_tmp_2 := src8_2_neg
          carry_bit  := 1
        }
        is(ALU_instr.ADD_H) {
          src8_tmp_2 := 0
          carry_bit  := alu_cout.asUInt
        }
        is(ALU_instr.INCR) {
          src8_tmp_2 := 0
          carry_bit  := 1
        }
        is(ALU_instr.SET_ONE) {
          src8_tmp_2 := 1
          carry_bit  := 0
        }
        default {
          src8_tmp_2 := 0
          carry_bit  := 0
        }
      }

      tmp9 := (src8_1_exp + src8_tmp_2 + carry_bit).asBits
    } otherwise {
      switch(instr) {
        //      is(ALU_instr.ADD) {
        //        tmp := (src8_1_exp + src8_2_exp).asBits
        //      }
        //      is(ALU_instr.SUB) {
        //        tmp := (src8_1_exp + (~src8_2_exp + 1)).asBits
        //      }
        //      is(ALU_instr.INCR) {
        //        tmp := (src8_1_exp + 1).asBits
        //      }
        //      is(ALU_instr.ADD_H) {
        //        tmp := (src8_1_exp + src8_2_exp + alu_cout.asSInt).asBits
        //      }
        is(ALU_instr.AND) {
          tmp9(7 downto 0) := src8_1 & src8_2
        }
        is(ALU_instr.OR) {
          tmp9(7 downto 0) := src8_1 | src8_2
        }
        is(ALU_instr.XOR) {
          tmp9(7 downto 0) := src8_1 ^ src8_2
        }
        default {
          tmp9 := 0
        }
      }
    }

    alu8                    := tmp9(7 downto 0)
    alu_cout                := tmp9.msb.asBits
    tmp_ccr(CCR.C.position) := tmp9.msb
    tmp_ccr(CCR.V.position) := False
    tmp_ccr(CCR.Z.position) := tmp9(7 downto 0) === 0
    tmp_ccr(CCR.N.position) := tmp9(7)
    tmp_ccr(CCR.I.position) := False
    tmp_ccr(CCR.H.position) := False
  }

  ALU(alu_instr)

  // Internal state
  val reset_state    = Reg(Bits(2 bits)) init(0)
  val cycle          = Reg(Bits(4 bits)) init(0)
  val end_instr_addr = Bits(16 bits)
  val end_instr_flag = Bits(1 bit)

  // Default values
  src8_1_select     := Reg8.NONE
  src8_2_select     := Reg8.NONE
  alu8_write        := Reg8.NONE.position
  alu_instr         := ALU_instr.NONE
  end_instr_addr    := 0
  end_instr_flag    := 0
  tmp16(7 downto 0) := 0

  reset_handler
  end_instr_flag_handler

  when(reset_state === 3) {
    when(cycle === 0) {
      fetch()
    } otherwise {
      execute()
    }
  }

  def reset_handler: Unit = {
    switch(reset_state) {
      is(0) {
        Addr        := B"16'hFFFE"
        RW          := 1
        reset_state := 1
      }
      is(1) {
        Addr        := B"16'hFFFF"
        RW          := 1
        tmp8        := io.Din
        reset_state := 2
      }
      is(2) {
        val reset_vector = Bits(16 bits)
        reset_vector := Cat(tmp8, io.Din)
        end_instr(reset_vector)
        reset_state := 3
      }
      default {
        reset_state := 3
      }
    }
  }

  def end_instr_flag_handler: Unit = {
    when(end_instr_flag === 1) {
      pc    := end_instr_addr
      Addr  := end_instr_addr
      RW    := 1
      cycle := 0
    }
  }

  def end_instr(addr: Bits): Unit = {
    end_instr_addr := addr
    end_instr_flag := 1
  }

  def fetch(): Unit = {
    instr := io.Din
    cycle := 1
    RW    := 1
    
    val new_pc = Bits(16 bits)
    new_pc := (pc.asSInt + 1).asBits
    pc    := new_pc
    Addr  := new_pc
  }

  def execute(): Unit = {
    src8_1_select := Reg8.NONE
    src8_2_select := Reg8.NONE
    alu_instr     := ALU_instr.NONE
    alu8_write    := Reg8.NONE().asBits

    tmp_ccr := 0

    a  := 0
    b  := 0
    sp := 0
    x  := 0
    tmp16 := 0

    switch(instr) {
//      is(0x01) { NOP() }
      is(0x08) { INX() }
//      is(0x0A) { CLV() }
//      is(0x0B) { SEV() }
//      is(0x0C) { CLC() }
//      is(0x0D) { SEC() }
//      is(0x0E) { CLI() }
//      is(0x0F) { SEI() }
//      is(0x1B) { ABA() }
//      is(0x20) { BRA() }
//      is(0x26) { BNE() }
//      is(0x27) { BEQ() }
//      is(0x28) { BVC() }
//      is(0x29) { BVS() }
//      is(0x7E) { JMPext() }
      default  { end_instr(pc) }
    }
  }

  def INX(): Unit = {
    val Z_16 = Reg(Bits(1 bit))
    when(cycle === 1) {
      // ALU inputs
      src8_1_select := Reg8.XL
      src8_2_select := Reg8.NONE
      alu8_write    := Reg8.XL().asBits
      alu_instr     := ALU_instr.SET_ONE

      // ALU output
      Z_16          := tmp_ccr(2).asBits

      cycle         := 2
    }
    when(cycle === 2) {
      src8_1_select := Reg8.XH
      src8_2_select := Reg8.XL
      alu8_write    := Reg8.XH().asBits
      alu_instr     := ALU_instr.ADD_H

      ccr_mask      := B"00000100"
      ccr           := ccr | (tmp_ccr & ccr_mask)
      ccr(3)        := ccr(3) | Z_16.asBool
      cycle := 3
    }
    when(cycle === 3) {
      alu8_write    := Reg8.NONE().asBits
      end_instr(pc)
    }
  }

  def ABA(): Unit = {
    when(cycle === 1) {
      src8_1_select := Reg8.A
      src8_2_select := Reg8.B
      alu8_write    := Reg8.A.position
      alu_instr     := ALU_instr.ADD

      ccr_mask := B"00101111".addAttribute("keep")
      ccr      := ccr | (tmp_ccr & ccr_mask)

//      ccr(CCR.Z.position) := (alu8 === 0)
//      ccr(CCR.N.position) := alu8.msb

//      val result = SInt(9 bits)
//      result := (a.asSInt.resize(result.getWidth) + b.asSInt.resize(result.getWidth))
//      a      := result(result.high - 1 downto 0).asBits
//
//      // Update flags
//      Z := (result(result.high - 1 downto 0) === 0).asBits
//      N := result(result.high - 1).asBits
//      C := result.msb.asBits
//
//      // Not finished, needed for SpinalHDL to compile
//      V := 0
//      H := 0
//      b := 0

      end_instr(pc)
    }
  }

  def NOP(): Unit = {
    end_instr(pc)
  }

  def CLC(): Unit = {
    when(cycle === 1) {
      ccr(CCR.C.position) := False
      end_instr(pc)
    }
  }

  def CLI(): Unit = {
    when(cycle === 1) {
      ccr(CCR.I.position) := False
      end_instr(pc)
    }
  }

  def CLV(): Unit = {
    when(cycle === 1) {
      ccr(CCR.V.position) := False
      end_instr(pc)
    }
  }

  def SEC(): Unit = {
    when(cycle === 1) {
      ccr(CCR.C.position) := True
      end_instr(pc)
    }
  }

  def SEI(): Unit = {
    when(cycle === 1) {
      ccr(CCR.I.position) := True
      end_instr(pc)
    }
  }

  def SEV(): Unit = {
    when(cycle === 1) {
      ccr(CCR.V.position) := True
      end_instr(pc)
    }
  }

  def BRA(): Unit = {
    when(cycle === 1) {
      tmp8  := io.Din

      // At this point, Addr is already incremented by one compared
      // to the address of the instruction itself, so only increment
      // by one to match the +2 in the documentation.
      tmp16 := (pc.asSInt + 1).asBits
      cycle := 2
    }
    when(cycle === 2) {
      tmp16 := (tmp16.asSInt + tmp8.asSInt).asBits
      cycle := 3
    }
    when(cycle === 3) {
      end_instr(tmp16)
    }
  }

  def BNE(): Unit = {
    when(cycle === 1) {
      tmp8  := io.Din

      // At this point, Addr is already incremented by one compared
      // to the address of the instruction itself, so only increment
      // by one to match the +2 in the documentation.
      tmp16 := (pc.asSInt + 1).asBits
      cycle := 2
    }
    when(cycle === 2) {
      tmp16 := (tmp16.asSInt + tmp8.asSInt).asBits
      cycle := 3
    }
    when(cycle === 3) {
      val new_pc = Bits(16 bits)

      when(ccr(CCR.Z.position) === False) {
        new_pc := tmp16
      } otherwise {
        // At this point, pc is already incremented by one compared
        // to the address of the instruction itself, so only increment
        // by one to match the +2 in the documentation.
        new_pc := (pc.asSInt + 1).asBits
      }
      end_instr(new_pc)
    }
  }

  def BEQ(): Unit = {
    when(cycle === 1) {
      tmp8  := io.Din

      // At this point, Addr is already incremented by one compared
      // to the address of the instruction itself, so only increment
      // by one to match the +2 in the documentation.
      tmp16 := (pc.asSInt + 1).asBits
      cycle := 2
    }
    when(cycle === 2) {
      tmp16 := (tmp16.asSInt + tmp8.asSInt).asBits
      cycle := 3
    }
    when(cycle === 3) {
      val new_pc = Bits(16 bits)

      when(ccr(CCR.Z.position) === True) {
        new_pc := tmp16
      } otherwise {
        // At this point, pc is already incremented by one compared
        // to the address of the instruction itself, so only increment
        // by one to match the +2 in the documentation.
        new_pc := (pc.asSInt + 1).asBits
      }
      end_instr(new_pc)
    }
  }

  def BVC(): Unit = {
    when(cycle === 1) {
      tmp8  := io.Din

      // At this point, Addr is already incremented by one compared
      // to the address of the instruction itself, so only increment
      // by one to match the +2 in the documentation.
      tmp16 := (pc.asSInt + 1).asBits
      cycle := 2
    }
    when(cycle === 2) {
      tmp16 := (tmp16.asSInt + tmp8.asSInt).asBits
      cycle := 3
    }
    when(cycle === 3) {
      val new_pc = Bits(16 bits)

      when(ccr(CCR.V.position) === False) {
        new_pc := tmp16
      } otherwise {
        // At this point, pc is already incremented by one compared
        // to the address of the instruction itself, so only increment
        // by one to match the +2 in the documentation.
        new_pc := (pc.asSInt + 1).asBits
      }
      end_instr(new_pc)
    }
  }

  def BVS(): Unit = {
    when(cycle === 1) {
      tmp8  := io.Din

      // At this point, Addr is already incremented by one compared
      // to the address of the instruction itself, so only increment
      // by one to match the +2 in the documentation.
      tmp16 := (pc.asSInt + 1).asBits
      cycle := 2
    }
    when(cycle === 2) {
      tmp16 := (tmp16.asSInt + tmp8.asSInt).asBits
      cycle := 3
    }
    when(cycle === 3) {
      val new_pc = Bits(16 bits)

      when(ccr(CCR.V.position) === True) {
        new_pc := tmp16
      } otherwise {
        // At this point, pc is already incremented by one compared
        // to the address of the instruction itself, so only increment
        // by one to match the +2 in the documentation.
        new_pc := (pc.asSInt + 1).asBits
      }
      end_instr(new_pc)
    }
  }

  def JMPext(): Unit = {
    when(cycle === 1) {
      tmp16(15 downto 8) := io.Din
      pc                 := (pc.asSInt + 1).asBits
      Addr               := (pc.asSInt + 1).asBits
      RW                 := 1
      cycle              := 2
    }
    when(cycle === 2) {
      val new_pc = Bits(16 bits)
      new_pc := Cat(tmp16(15 downto 8), io.Din)
      end_instr(new_pc)
    }
  }

  // IO wiring
  io.RW   := RW
  io.Addr := Addr
  io.Dout := Dout

//  val f_past_valid = Reg(Bool)
//  f_past_valid := True

//  val f_cntr = Reg(UInt(10 bits)) init (0)
//  f_cntr := f_cntr + 1
//
//  import spinal.core.Formal._
//
//  GenerationFlags.formal {
//    when(initstate()) {
//      assume(clockDomain.isResetActive)
//    }.otherwise {
//      when(f_cntr === 20) {
//        cover(instr === B"01111110")
//      }
//      when(past(clockDomain.isResetActive, 4) && !past(clockDomain.isResetActive, 3)
//        && !past(clockDomain.isResetActive, 2) && !past(clockDomain.isResetActive))
//      {
//        assert(past(Addr, 2) === 0xFFFE)
//        assert(past(Addr) === 0xFFFF)
//        assert(Addr(15 downto 8) === past(io.Din, 2))
//        assert(Addr === pc)
//      }
//
//      // Check CLC, CLV, and CLI instructions
//      when(!past(clockDomain.isResetActive) && past(reset_state) === 3 && past(cycle) === 1) {
//        when(past(instr) === 0x0C) {
//          assert(ccr(CCR.C.position) === False)
//        }
//        when(past(instr) === 0x0E) {
//          assert(ccr(CCR.I.position) === False)
//        }
//        when(past(instr) === 0x0A) {
//          assert(ccr(CCR.V.position) === False)
//        }
//      }
//
//      // Check SEC, SEV, and SEI instructions
//      when(!past(clockDomain.isResetActive) && past(reset_state) === 3 && past(cycle) === 1) {
//        when(past(instr) === 0x0D) {
//          assert(ccr(CCR.C.position) === True)
//        }
//        when(past(instr) === 0x0F) {
//          assert(ccr(CCR.I.position) === True)
//        }
//        when(past(instr) === 0x0B) {
//          assert(ccr(CCR.V.position) === True)
//        }
//      }
//
//      // Check branch instructions
//      when(!past(clockDomain.isResetActive) && past(reset_state) === 3 && past(cycle) === 3) {
//                // Check BRA instruction
//                when(past(instr, 3) === 0x20) {
//                  // We have to resize to make all signals the same size, or else the $past statement will be outside clocked
//                  // always blocks.
//                  assert(io.Addr === (past(io.Din.asSInt.resize(Addr.getWidth), 3) + past(pc.asSInt.resize(Addr.getWidth), 4) + 2).asBits)
//                }
//
//        // Check BNE instruction
//        when(past(instr, 3) === 0x26) {
//          when(past(ccr(CCR.Z.position)) === False) {
//            // We have to resize to make all signals the same size, or else the $past statement will be outside clocked
//            // always blocks.
//            assert(io.Addr === (past(io.Din.asSInt.resize(Addr.getWidth), 3) + past(pc.asSInt.resize(Addr.getWidth), 4) + 2).asBits)
//          } otherwise {
//            assert(io.Addr === (past(pc.asSInt.resize(Addr.getWidth), 4) + 2).asBits)
//          }
//        }
//
//        // Check BEQ instruction
//        when(past(instr, 3) === 0x27) {
//          when(past(ccr(CCR.Z.position)) === True) {
//            // We have to resize to make all signals the same size, or else the $past statement will be outside clocked
//            // always blocks.
//            assert(io.Addr === (past(io.Din.asSInt.resize(Addr.getWidth), 3) + past(pc.asSInt.resize(Addr.getWidth), 4) + 2).asBits)
//          } otherwise {
//            assert(io.Addr === (past(pc.asSInt.resize(Addr.getWidth), 4) + 2).asBits)
//          }
//        }
//
//        // Check BVC instruction
//        when(past(instr, 3) === 0x28) {
//          when(past(ccr(CCR.V.position)) === False) {
//            // We have to resize to make all signals the same size, or else the $past statement will be outside clocked
//            // always blocks.
//            assert(io.Addr === (past(io.Din.asSInt.resize(Addr.getWidth), 3) + past(pc.asSInt.resize(Addr.getWidth), 4) + 2).asBits)
//          } otherwise {
//            assert(io.Addr === (past(pc.asSInt.resize(Addr.getWidth), 4) + 2).asBits)
//          }
//        }
//
//        // Check BVS instruction
//        when(past(instr, 3) === 0x29) {
//          when(past(ccr(CCR.V.position)) === True) {
//            // We have to resize to make all signals the same size, or else the $past statement will be outside clocked
//            // always blocks.
//            assert(io.Addr === (past(io.Din.asSInt.resize(Addr.getWidth), 3) + past(pc.asSInt.resize(Addr.getWidth), 4) + 2).asBits)
//          } otherwise {
//            assert(io.Addr === (past(pc.asSInt.resize(Addr.getWidth), 4) + 2).asBits)
//          }
//        }
//      }
//
//      // Check jump instructions
//      when(!past(clockDomain.isResetActive) && past(reset_state) === 3 && past(cycle) === 2) {
//        // Check JMP ext instruction
//        when(past(instr, 2) === 0x7E) {
//          assert(io.Addr === (Cat(past(io.Din, 2), past(io.Din))))
//        }
//      }
//    }
//  }
}

object CoreVerilog {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new Core).printPruned()
  }
}

object CoreVerilogWithFormal {
  def main(args: Array[String]): Unit = {
    val config = SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH))
    config.includeFormal.generateSystemVerilog(new Core())
  }
}