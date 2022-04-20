
module Emulator.Opcode
  ( Mnemonic(..)
  , AddressMode(..)
  , Opcode(..)
  , decodeOpcode
  ) where

import           Data.List                      ( intersperse )
import           Data.Word
import           Emulator.Util

data AddressMode
  = IMP
  | ACC
  | IMM
  | ZP
  | ZPX
  | ZPY
  | REL
  | ABS
  | ABX
  | ABY
  | IND
  | IZX
  | IZY
    deriving (Show, Eq)

data Mnemonic
  = ADC | AND | ASL | BCC | BCS | BEQ
  | BIT | BMI | BNE | BPL | BRK | BVC
  | BVS | CLC | CLD | CLI | CLV | CMP
  | CPX | CPY | DEC | DEX | DEY | EOR
  | INC | INX | INY | JMP | JSR | LDA
  | LDX | LDY | LSR | NOP | ORA | PHA
  | PHP | PLA | PLP | ROL | ROR | RTI
  | RTS | SBC | SEC | SED | SEI | STA
  | STX | STY | TAX | TAY | TSX | TXA
  | TXS | TYA
  | KIL | LAX | SAX | DCP | ISC | RLA
  | RRA | SLO | SRE | ANC | ALR | ARR
  | XAA | AHX | TAS | SHX | SHY | LAS
  | AXS
    deriving (Show, Read, Eq, Enum)

data Opcode = Opcode
  { raw             :: Word8
  , mnem            :: Mnemonic
  , mode            :: AddressMode
  , len             :: Int
  , cycles          :: Int
  , pageCrossCycles :: Int
  }
  deriving Eq

instance Show Opcode where
  show (Opcode raw mn mode len cyc pageCrossCy) =
    concat
      . intersperse " "
      $ "Opcode:"
      : prettifyWord raw
      : show mn
      : show mode
      : map show [len, cyc, pageCrossCy]

decodeOpcode :: Word8 -> Opcode
decodeOpcode w = Opcode w mnemonic addressMode length cycles pageCrossCycles
 where
  (mnemonic, addressMode, length, cycles, pageCrossCycles) = case w of
    0x69  -> (ADC, IMM, 2, 2, 0)
    0x65  -> (ADC, ZP, 2, 3, 0)
    0x75  -> (ADC, ZPX, 2, 4, 0)
    0x6D  -> (ADC, ABS, 3, 4, 0)
    0x7D  -> (ADC, ABX, 3, 4, 1)
    0x79  -> (ADC, ABY, 3, 4, 1)
    0x61  -> (ADC, IZX, 2, 6, 0)
    0x71  -> (ADC, IZY, 2, 5, 1)
    0x29  -> (AND, IMM, 2, 2, 0)
    0x25  -> (AND, ZP, 2, 3, 0)
    0x35  -> (AND, ZPX, 2, 4, 0)
    0x2D  -> (AND, ABS, 3, 4, 0)
    0x3D  -> (AND, ABX, 3, 4, 1)
    0x39  -> (AND, ABY, 3, 4, 1)
    0x21  -> (AND, IZX, 2, 6, 0)
    0x31  -> (AND, IZY, 2, 5, 1)
    0x0A  -> (ASL, ACC, 1, 2, 0)
    0x06  -> (ASL, ZP, 2, 5, 0)
    0x16  -> (ASL, ZPX, 2, 6, 0)
    0x0E  -> (ASL, ABS, 3, 6, 0)
    0x1E  -> (ASL, ABX, 3, 7, 0)
    0x90  -> (BCC, REL, 2, 2, 0)
    0xB0  -> (BCS, REL, 2, 2, 0)
    0xF0  -> (BEQ, REL, 2, 2, 1)
    0x24  -> (BIT, ZP, 2, 3, 0)
    0x2C  -> (BIT, ABS, 3, 4, 0)
    0x30  -> (BMI, REL, 2, 2, 0)
    0xD0  -> (BNE, REL, 2, 2, 0)
    0x10  -> (BPL, REL, 2, 2, 0)
    0x00  -> (BRK, IMP, 1, 7, 0)
    0x50  -> (BVC, REL, 2, 2, 0)
    0x70  -> (BVS, REL, 2, 2, 0)
    0x18  -> (CLC, IMP, 1, 2, 0)
    0xD8  -> (CLD, IMP, 1, 2, 0)
    0x58  -> (CLI, IMP, 1, 2, 0)
    0xB8  -> (CLV, IMP, 1, 2, 0)
    0xC9  -> (CMP, IMM, 2, 2, 0)
    0xC5  -> (CMP, ZP, 2, 3, 0)
    0xD5  -> (CMP, ZPX, 2, 4, 0)
    0xCD  -> (CMP, ABS, 3, 4, 0)
    0xDD  -> (CMP, ABX, 3, 4, 1)
    0xD9  -> (CMP, ABY, 3, 4, 1)
    0xC1  -> (CMP, IZX, 2, 6, 0)
    0xD1  -> (CMP, IZY, 2, 5, 1)
    0xE0  -> (CPX, IMM, 2, 2, 0)
    0xE4  -> (CPX, ZP, 2, 3, 0)
    0xEC  -> (CPX, ABS, 3, 4, 0)
    0xC0  -> (CPY, IMM, 2, 2, 0)
    0xC4  -> (CPY, ZP, 2, 3, 0)
    0xCC  -> (CPY, ABS, 3, 4, 0)
    0xC6  -> (DEC, ZP, 2, 5, 0)
    0xD6  -> (DEC, ZPX, 2, 6, 0)
    0xCE  -> (DEC, ABS, 3, 6, 0)
    0xDE  -> (DEC, ABX, 3, 7, 0)
    0xCA  -> (DEX, IMP, 1, 2, 0)
    0x88  -> (DEY, IMP, 1, 2, 0)
    0x49  -> (EOR, IMM, 2, 2, 0)
    0x45  -> (EOR, ZP, 2, 3, 0)
    0x55  -> (EOR, ZPX, 2, 4, 0)
    0x4D  -> (EOR, ABS, 3, 4, 0)
    0x5D  -> (EOR, ABX, 3, 4, 1)
    0x59  -> (EOR, ABY, 3, 4, 1)
    0x41  -> (EOR, IZX, 2, 6, 0)
    0x51  -> (EOR, IZY, 2, 5, 1)
    0xE6  -> (INC, ZP, 2, 5, 0)
    0xF6  -> (INC, ZPX, 2, 6, 0)
    0xEE  -> (INC, ABS, 3, 6, 0)
    0xFE  -> (INC, ABX, 3, 7, 0)
    0xE8  -> (INX, IMP, 1, 2, 0)
    0xC8  -> (INY, IMP, 1, 2, 0)
    0x4C  -> (JMP, ABS, 3, 3, 0)
    0x6C  -> (JMP, IND, 3, 5, 0)
    0x20  -> (JSR, ABS, 3, 6, 0)
    0xA9  -> (LDA, IMM, 2, 2, 0)
    0xA5  -> (LDA, ZP, 2, 3, 0)
    0xB5  -> (LDA, ZPX, 2, 4, 0)
    0xAD  -> (LDA, ABS, 3, 4, 0)
    0xBD  -> (LDA, ABX, 3, 4, 1)
    0xB9  -> (LDA, ABY, 3, 4, 1)
    0xA1  -> (LDA, IZX, 2, 6, 0)
    0xB1  -> (LDA, IZY, 2, 5, 1)
    0xA2  -> (LDX, IMM, 2, 2, 0)
    0xA6  -> (LDX, ZP, 2, 3, 0)
    0xB6  -> (LDX, ZPY, 2, 4, 0)
    0xAE  -> (LDX, ABS, 3, 4, 0)
    0xBE  -> (LDX, ABY, 3, 4, 1)
    0xA0  -> (LDY, IMM, 2, 2, 0)
    0xA4  -> (LDY, ZP, 2, 3, 0)
    0xB4  -> (LDY, ZPX, 2, 4, 0)
    0xAC  -> (LDY, ABS, 3, 4, 0)
    0xBC  -> (LDY, ABX, 3, 4, 1)
    0x4A  -> (LSR, ACC, 1, 2, 0)
    0x46  -> (LSR, ZP, 2, 5, 0)
    0x56  -> (LSR, ZPX, 2, 6, 0)
    0x4E  -> (LSR, ABS, 3, 6, 0)
    0x5E  -> (LSR, ABX, 3, 7, 0)
    0xEA  -> (NOP, IMP, 1, 2, 0)
    0x09  -> (ORA, IMM, 2, 2, 0)
    0x05  -> (ORA, ZP, 2, 3, 0)
    0x15  -> (ORA, ZPX, 2, 4, 0)
    0x0D  -> (ORA, ABS, 3, 4, 0)
    0x1D  -> (ORA, ABX, 3, 4, 1)
    0x19  -> (ORA, ABY, 3, 4, 1)
    0x01  -> (ORA, IZX, 2, 6, 0)
    0x11  -> (ORA, IZY, 2, 5, 1)
    0x48  -> (PHA, IMP, 1, 3, 0)
    0x08  -> (PHP, IMP, 1, 3, 0)
    0x68  -> (PLA, IMP, 1, 4, 0)
    0x28  -> (PLP, IMP, 1, 4, 0)
    0x2A  -> (ROL, ACC, 1, 2, 0)
    0x26  -> (ROL, ZP, 2, 5, 0)
    0x36  -> (ROL, ZPX, 2, 6, 0)
    0x2E  -> (ROL, ABS, 3, 6, 0)
    0x3E  -> (ROL, ABX, 3, 7, 0)
    0x6A  -> (ROR, ACC, 1, 2, 0)
    0x66  -> (ROR, ZP, 2, 5, 0)
    0x76  -> (ROR, ZPX, 2, 6, 0)
    0x6E  -> (ROR, ABS, 3, 6, 0)
    0x7E  -> (ROR, ABX, 3, 7, 0)
    0x40  -> (RTI, IMP, 1, 6, 0)
    0x60  -> (RTS, IMP, 1, 6, 0)
    0xE9  -> (SBC, IMM, 2, 2, 0)
    0xE5  -> (SBC, ZP, 2, 3, 0)
    0xF5  -> (SBC, ZPX, 2, 4, 0)
    0xED  -> (SBC, ABS, 3, 4, 0)
    0xFD  -> (SBC, ABX, 3, 4, 1)
    0xF9  -> (SBC, ABY, 3, 4, 1)
    0xE1  -> (SBC, IZX, 2, 6, 0)
    0xF1  -> (SBC, IZY, 2, 5, 1)
    0x38  -> (SEC, IMP, 1, 2, 0)
    0xF8  -> (SED, IMP, 1, 2, 0)
    0x78  -> (SEI, IMP, 1, 2, 0)
    0x85  -> (STA, ZP, 2, 3, 0)
    0x95  -> (STA, ZPX, 2, 4, 0)
    0x8D  -> (STA, ABS, 3, 4, 0)
    0x9D  -> (STA, ABX, 3, 5, 0)
    0x99  -> (STA, ABY, 3, 5, 0)
    0x81  -> (STA, IZX, 2, 6, 0)
    0x91  -> (STA, IZY, 2, 6, 0)
    0x86  -> (STX, ZP, 2, 3, 0)
    0x96  -> (STX, ZPY, 2, 4, 0)
    0x8E  -> (STX, ABS, 3, 4, 0)
    0x84  -> (STY, ZP, 2, 3, 0)
    0x94  -> (STY, ZPX, 2, 4, 0)
    0x8C  -> (STY, ABS, 3, 4, 0)
    0xAA  -> (TAX, IMP, 1, 2, 0)
    0xA8  -> (TAY, IMP, 1, 2, 0)
    0xBA  -> (TSX, IMP, 1, 2, 0)
    0x8A  -> (TXA, IMP, 1, 2, 0)
    0x9A  -> (TXS, IMP, 1, 2, 0)
    0x98  -> (TYA, IMP, 1, 2, 0)

    0x0B  -> (ANC, IMM, 2, 2, 0)
    0x2B  -> (ANC, IMM, 2, 2, 0)
    0x87  -> (SAX, ZP, 2, 3, 0)
    0x97  -> (SAX, ZPY, 2, 4, 0)
    0x83  -> (SAX, IZX, 2, 6, 0)
    0x8F  -> (SAX, ABS, 3, 4, 0)
    0x6B  -> (ARR, IMM, 2, 2, 0)
    0x4B  -> (ALR, IMM, 2, 2, 0)
    0xAB  -> (LAX, IMM, 2, 2, 0)
    0x9F  -> (AHX, ABY, 3, 5, 0)
    0x93  -> (AHX, IZY, 2, 6, 0)
    0xCB  -> (AXS, IMM, 2, 2, 0)
    0xC7  -> (DCP, ZP, 2, 5, 0)
    0xD7  -> (DCP, ZPX, 2, 6, 0)
    0xCF  -> (DCP, ABS, 3, 6, 0)
    0xDF  -> (DCP, ABX, 3, 7, 0)
    0xDB  -> (DCP, ABY, 3, 7, 0)
    0xC3  -> (DCP, IZX, 2, 8, 0)
    0x14  -> (NOP, ZPX, 2, 4, 0)
    0x34  -> (NOP, ZPX, 2, 4, 0)
    0x44  -> (NOP, ZP, 2, 3, 0)
    0x54  -> (NOP, ZPX, 2, 4, 0)
    0x64  -> (NOP, ZP, 2, 3, 0)
    0x74  -> (NOP, ZPX, 2, 4, 0)
    0x80  -> (NOP, IMM, 2, 2, 0)
    0x82  -> (NOP, IMM, 2, 2, 0)
    0x89  -> (NOP, IMM, 2, 2, 0)
    0xC2  -> (NOP, IMM, 2, 2, 0)
    0xD4  -> (NOP, ZPX, 2, 4, 0)
    0xE2  -> (NOP, IMM, 2, 2, 0)
    0xF4  -> (NOP, ZPX, 2, 4, 0)
    0xE7  -> (ISC, ZP, 2, 5, 0)
    0xF7  -> (ISC, ZPX, 2, 6, 0)
    0xEF  -> (ISC, ABS, 3, 6, 0)
    0xFF  -> (ISC, ABX, 3, 7, 0)
    0xFB  -> (ISC, ABY, 3, 7, 0)
    0xE3  -> (ISC, IZX, 2, 8, 0)
    0xF3  -> (ISC, IZY, 2, 8, 0)
    0x02  -> (KIL, IMP, 1, 0, 0)
    0x12  -> (KIL, IMP, 1, 0, 0)
    0x22  -> (KIL, IMP, 1, 0, 0)
    0x32  -> (KIL, IMP, 1, 0, 0)
    0x42  -> (KIL, IMP, 1, 0, 0)
    0x52  -> (KIL, IMP, 1, 0, 0)
    0x62  -> (KIL, IMP, 1, 0, 0)
    0x72  -> (KIL, IMP, 1, 0, 0)
    0x92  -> (KIL, IMP, 1, 0, 0)
    0xB2  -> (KIL, IMP, 1, 0, 0)
    0xD2  -> (KIL, IMP, 1, 0, 0)
    0xF2  -> (KIL, IMP, 1, 0, 0)
    0xBB  -> (LAS, ABY, 3, 4, 1)
    0xA7  -> (LAX, ZP, 2, 3, 0)
    0xB7  -> (LAX, ZPY, 2, 4, 0)
    0xAF  -> (LAX, ABS, 3, 4, 0)
    0xBF  -> (LAX, ABY, 3, 4, 1)
    0xA3  -> (LAX, IZX, 2, 6, 0)
    0xB3  -> (LAX, IZY, 2, 5, 1)
    0x1A  -> (NOP, IMP, 1, 2, 0)
    0x3A  -> (NOP, IMP, 1, 2, 0)
    0x5A  -> (NOP, IMP, 1, 2, 0)
    0x7A  -> (NOP, IMP, 1, 2, 0)
    0xDA  -> (NOP, IMP, 1, 2, 0)
    0xFA  -> (NOP, IMP, 1, 2, 0)
    0x27  -> (RLA, ZP, 2, 5, 0)
    0x37  -> (RLA, ZPX, 2, 6, 0)
    0x2F  -> (RLA, ABS, 3, 6, 0)
    0x3F  -> (RLA, ABX, 3, 7, 0)
    0x3B  -> (RLA, ABY, 3, 7, 0)
    0x23  -> (RLA, IZX, 2, 8, 0)
    0x33  -> (RLA, IZY, 2, 8, 0)
    0x67  -> (RRA, ZP, 2, 5, 0)
    0x77  -> (RRA, ZPX, 2, 6, 0)
    0x6F  -> (RRA, ABS, 3, 6, 0)
    0x7F  -> (RRA, ABX, 3, 7, 0)
    0x7B  -> (RRA, ABY, 3, 7, 0)
    0x63  -> (RRA, IZX, 2, 8, 0)
    0x73  -> (RRA, IZY, 2, 8, 0)
    0xEB  -> (SBC, IMM, 2, 2, 0)
    0x07  -> (SLO, ZP, 2, 5, 0)
    0x17  -> (SLO, ZPX, 2, 6, 0)
    0x0F  -> (SLO, ABS, 3, 6, 0)
    0x1F  -> (SLO, ABX, 3, 7, 0)
    0x1B  -> (SLO, ABY, 3, 7, 0)
    0x03  -> (SLO, IZX, 2, 8, 0)
    0x13  -> (SLO, IZY, 2, 8, 0)
    0x47  -> (SRE, ZP, 2, 5, 0)
    0x57  -> (SRE, ZPX, 2, 6, 0)
    0x4F  -> (SRE, ABS, 3, 6, 0)
    0x5F  -> (SRE, ABX, 3, 7, 0)
    0x5B  -> (SRE, ABY, 3, 7, 0)
    0x43  -> (SRE, IZX, 2, 8, 0)
    0x53  -> (SRE, IZY, 2, 8, 0)
    0x9E  -> (SHX, ABY, 3, 5, 0)
    0x9C  -> (SHY, ABX, 3, 5, 0)
    0x0C  -> (NOP, ABS, 3, 4, 0)
    0x1C  -> (NOP, ABX, 3, 4, 1)
    0x3C  -> (NOP, ABX, 3, 4, 1)
    0x5C  -> (NOP, ABX, 3, 4, 1)
    0x7C  -> (NOP, ABX, 3, 4, 1)
    0xDC  -> (NOP, ABX, 3, 4, 1)
    0xFC  -> (NOP, ABX, 3, 4, 1)
    0xD3  -> (DCP, IZY, 2, 8, 0)
    0x04  -> (NOP, ZP, 2, 3, 0)
    0x8B  -> (XAA, IMM, 2, 2, 0)
    0x9B  -> (TAS, ABY, 3, 5, 0)
    other -> error $ show other ++ " is not a known opcode"
