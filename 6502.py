import numpy as np


class INSTRUCTION:
    def __init__(self, data):
        self.name = data[0]
        self.address_mode = data[1]
        self.operate = data[2]
        self.cycles = data[3]


# Bus
class Bus:
    def __init__(self):
        self.data = 0x00
        self.address = 0x0000

        self.ram = np.zeros(0xFFFF, dtype=np.uint8)

    def write(self, address, data):
        self.ram[address] = data
        return 0x00

    def read(self, address, readOnly=False):
        return self.ram[address]

class cpu6502:

    # Initialize the addressing mode functions
    def IMP(self): pass
    def IMM(self): pass
    def ZP0(self): pass
    def ZPX(self): pass
    def ZPY(self): pass
    def REL(self): pass
    def ABS(self): pass
    def ABX(self): pass
    def ABY(self): pass
    def IND(self): pass
    def IZX(self): pass
    def IZY(self): pass

    # Initialize the instruction set
    def ADC(self): pass
    def AND(self): pass
    def ASL(self): pass
    def BCC(self): pass
    def BCS(self): pass
    def BEQ(self): pass
    def BIT(self): pass
    def BMI(self): pass
    def BNE(self): pass
    def BPL(self): pass
    def BRK(self): pass
    def BVC(self): pass
    def BVS(self): pass
    def CLC(self): pass
    def CLD(self): pass
    def CLI(self): pass
    def CLV(self): pass
    def CMP(self): pass
    def CPX(self): pass
    def CPY(self): pass
    def DEC(self): pass
    def DEX(self): pass
    def DEY(self): pass
    def EOR(self): pass
    def INC(self): pass
    def INX(self): pass
    def INY(self): pass
    def JMP(self): pass
    def JSR(self): pass
    def LDA(self): pass
    def LDX(self): pass
    def LDY(self): pass
    def LSR(self): pass
    def NOP(self): pass
    def ORA(self): pass
    def PHA(self): pass
    def PHP(self): pass
    def PLA(self): pass
    def PLP(self): pass
    def ROL(self): pass
    def ROR(self): pass
    def RTI(self): pass
    def RTS(self): pass
    def SBC(self): pass
    def SEC(self): pass
    def SED(self): pass
    def SEI(self): pass
    def STA(self): pass
    def STX(self): pass
    def STY(self): pass
    def TAX(self): pass
    def TAY(self): pass
    def TSX(self): pass
    def TXA(self): pass
    def TXS(self): pass
    def TYA(self): pass
    def XXX(self): pass

    def __init__(self):

        self.bus = None  # Initialize the bus variable
        self.fetched = 0x00
        self.addr_abs = 0x0000
        self.addr_rel = 0x00
        self.opcode = 0x00
        self.cycles = 0

        self.FLAGS6502 = {
            "C": (1 << 0),  # Carry flag
            "Z": (1 << 1),  # Zero flag
            "I": (1 << 2),  # Disable Interrupts
            "D": (1 << 3),  # Decimal mode (unused)
            "B": (1 << 4),  # Break
            "U": (1 << 5),  # Unused
            "V": (1 << 6),  # Overflow flag
            "N": (1 << 7)  # Negative flag
        }

        self.A = 0x00  # Accumulator
        self.X = 0x00  # X Register
        self.Y = 0x00  # Y Register
        self.STKP = 0x00  # Stack Pointer
        self.PC = 0x0000  # Program Counter
        self.status = 0x00  # Processor status

        self.codes = [[ "BRK", self.BRK, self.IMM, 7 ],[ "ORA", self.ORA, self.IZX, 6 ],[ "???", self.XXX, self.IMP, 2 ],[ "???", self.XXX, self.IMP, 8 ],[ "???", self.NOP, self.IMP, 3 ],[ "ORA", self.ORA, self.ZP0, 3 ],[ "ASL", self.ASL, self.ZP0, 5 ],[ "???", self.XXX, self.IMP, 5 ],[ "PHP", self.PHP, self.IMP, 3 ],[ "ORA", self.ORA, self.IMM, 2 ],[ "ASL", self.ASL, self.IMP, 2 ],[ "???", self.XXX, self.IMP, 2 ],[ "???", self.NOP, self.IMP, 4 ],[ "ORA", self.ORA, self.ABS, 4 ],[ "ASL", self.ASL, self.ABS, 6 ],[ "???", self.XXX, self.IMP, 6 ],
                      [ "BPL", self.BPL, self.REL, 2 ],[ "ORA", self.ORA, self.IZY, 5 ],[ "???", self.XXX, self.IMP, 2 ],[ "???", self.XXX, self.IMP, 8 ],[ "???", self.NOP, self.IMP, 4 ],[ "ORA", self.ORA, self.ZPX, 4 ],[ "ASL", self.ASL, self.ZPX, 6 ],[ "???", self.XXX, self.IMP, 6 ],[ "CLC", self.CLC, self.IMP, 2 ],[ "ORA", self.ORA, self.ABY, 4 ],[ "???", self.NOP, self.IMP, 2 ],[ "???", self.XXX, self.IMP, 7 ],[ "???", self.NOP, self.IMP, 4 ],[ "ORA", self.ORA, self.ABX, 4 ],[ "ASL", self.ASL, self.ABX, 7 ],[ "???", self.XXX, self.IMP, 7 ],
                      [ "JSR", self.JSR, self.ABS, 6 ],[ "AND", self.AND, self.IZX, 6 ],[ "???", self.XXX, self.IMP, 2 ],[ "???", self.XXX, self.IMP, 8 ],[ "BIT", self.BIT, self.ZP0, 3 ],[ "AND", self.AND, self.ZP0, 3 ],[ "ROL", self.ROL, self.ZP0, 5 ],[ "???", self.XXX, self.IMP, 5 ],[ "PLP", self.PLP, self.IMP, 4 ],[ "AND", self.AND, self.IMM, 2 ],[ "ROL", self.ROL, self.IMP, 2 ],[ "???", self.XXX, self.IMP, 2 ],[ "BIT", self.BIT, self.ABS, 4 ],[ "AND", self.AND, self.ABS, 4 ],[ "ROL", self.ROL, self.ABS, 6 ],[ "???", self.XXX, self.IMP, 6 ],
                      [ "BMI", self.BMI, self.REL, 2 ],[ "AND", self.AND, self.IZY, 5 ],[ "???", self.XXX, self.IMP, 2 ],[ "???", self.XXX, self.IMP, 8 ],[ "???", self.NOP, self.IMP, 4 ],[ "AND", self.AND, self.ZPX, 4 ],[ "ROL", self.ROL, self.ZPX, 6 ],[ "???", self.XXX, self.IMP, 6 ],[ "SEC", self.SEC, self.IMP, 2 ],[ "AND", self.AND, self.ABY, 4 ],[ "???", self.NOP, self.IMP, 2 ],[ "???", self.XXX, self.IMP, 7 ],[ "???", self.NOP, self.IMP, 4 ],[ "AND", self.AND, self.ABX, 4 ],[ "ROL", self.ROL, self.ABX, 7 ],[ "???", self.XXX, self.IMP, 7 ],
                      [ "RTI", self.RTI, self.IMP, 6 ],[ "EOR", self.EOR, self.IZX, 6 ],[ "???", self.XXX, self.IMP, 2 ],[ "???", self.XXX, self.IMP, 8 ],[ "???", self.NOP, self.IMP, 3 ],[ "EOR", self.EOR, self.ZP0, 3 ],[ "LSR", self.LSR, self.ZP0, 5 ],[ "???", self.XXX, self.IMP, 5 ],[ "PHA", self.PHA, self.IMP, 3 ],[ "EOR", self.EOR, self.IMM, 2 ],[ "LSR", self.LSR, self.IMP, 2 ],[ "???", self.XXX, self.IMP, 2 ],[ "JMP", self.JMP, self.ABS, 3 ],[ "EOR", self.EOR, self.ABS, 4 ],[ "LSR", self.LSR, self.ABS, 6 ],[ "???", self.XXX, self.IMP, 6 ],
                      [ "BVC", self.BVC, self.REL, 2 ],[ "EOR", self.EOR, self.IZY, 5 ],[ "???", self.XXX, self.IMP, 2 ],[ "???", self.XXX, self.IMP, 8 ],[ "???", self.NOP, self.IMP, 4 ],[ "EOR", self.EOR, self.ZPX, 4 ],[ "LSR", self.LSR, self.ZPX, 6 ],[ "???", self.XXX, self.IMP, 6 ],[ "CLI", self.CLI, self.IMP, 2 ],[ "EOR", self.EOR, self.ABY, 4 ],[ "???", self.NOP, self.IMP, 2 ],[ "???", self.XXX, self.IMP, 7 ],[ "???", self.NOP, self.IMP, 4 ],[ "EOR", self.EOR, self.ABX, 4 ],[ "LSR", self.LSR, self.ABX, 7 ],[ "???", self.XXX, self.IMP, 7 ],
                      [ "RTS", self.RTS, self.IMP, 6 ],[ "ADC", self.ADC, self.IZX, 6 ],[ "???", self.XXX, self.IMP, 2 ],[ "???", self.XXX, self.IMP, 8 ],[ "???", self.NOP, self.IMP, 3 ],[ "ADC", self.ADC, self.ZP0, 3 ],[ "ROR", self.ROR, self.ZP0, 5 ],[ "???", self.XXX, self.IMP, 5 ],[ "PLA", self.PLA, self.IMP, 4 ],[ "ADC", self.ADC, self.IMM, 2 ],[ "ROR", self.ROR, self.IMP, 2 ],[ "???", self.XXX, self.IMP, 2 ],[ "JMP", self.JMP, self.IND, 5 ],[ "ADC", self.ADC, self.ABS, 4 ],[ "ROR", self.ROR, self.ABS, 6 ],[ "???", self.XXX, self.IMP, 6 ],
                      [ "BVS", self.BVS, self.REL, 2 ],[ "ADC", self.ADC, self.IZY, 5 ],[ "???", self.XXX, self.IMP, 2 ],[ "???", self.XXX, self.IMP, 8 ],[ "???", self.NOP, self.IMP, 4 ],[ "ADC", self.ADC, self.ZPX, 4 ],[ "ROR", self.ROR, self.ZPX, 6 ],[ "???", self.XXX, self.IMP, 6 ],[ "SEI", self.SEI, self.IMP, 2 ],[ "ADC", self.ADC, self.ABY, 4 ],[ "???", self.NOP, self.IMP, 2 ],[ "???", self.XXX, self.IMP, 7 ],[ "???", self.NOP, self.IMP, 4 ],[ "ADC", self.ADC, self.ABX, 4 ],[ "ROR", self.ROR, self.ABX, 7 ],[ "???", self.XXX, self.IMP, 7 ],
                      [ "???", self.NOP, self.IMP, 2 ],[ "STA", self.STA, self.IZX, 6 ],[ "???", self.NOP, self.IMP, 2 ],[ "???", self.XXX, self.IMP, 6 ],[ "STY", self.STY, self.ZP0, 3 ],[ "STA", self.STA, self.ZP0, 3 ],[ "STX", self.STX, self.ZP0, 3 ],[ "???", self.XXX, self.IMP, 3 ],[ "DEY", self.DEY, self.IMP, 2 ],[ "???", self.NOP, self.IMP, 2 ],[ "TXA", self.TXA, self.IMP, 2 ],[ "???", self.XXX, self.IMP, 2 ],[ "STY", self.STY, self.ABS, 4 ],[ "STA", self.STA, self.ABS, 4 ],[ "STX", self.STX, self.ABS, 4 ],[ "???", self.XXX, self.IMP, 4 ],
                      [ "BCC", self.BCC, self.REL, 2 ],[ "STA", self.STA, self.IZY, 6 ],[ "???", self.XXX, self.IMP, 2 ],[ "???", self.XXX, self.IMP, 6 ],[ "STY", self.STY, self.ZPX, 4 ],[ "STA", self.STA, self.ZPX, 4 ],[ "STX", self.STX, self.ZPY, 4 ],[ "???", self.XXX, self.IMP, 4 ],[ "TYA", self.TYA, self.IMP, 2 ],[ "STA", self.STA, self.ABY, 5 ],[ "TXS", self.TXS, self.IMP, 2 ],[ "???", self.XXX, self.IMP, 5 ],[ "???", self.NOP, self.IMP, 5 ],[ "STA", self.STA, self.ABX, 5 ],[ "???", self.XXX, self.IMP, 5 ],[ "???", self.XXX, self.IMP, 5 ],
                      [ "LDY", self.LDY, self.IMM, 2 ],[ "LDA", self.LDA, self.IZX, 6 ],[ "LDX", self.LDX, self.IMM, 2 ],[ "???", self.XXX, self.IMP, 6 ],[ "LDY", self.LDY, self.ZP0, 3 ],[ "LDA", self.LDA, self.ZP0, 3 ],[ "LDX", self.LDX, self.ZP0, 3 ],[ "???", self.XXX, self.IMP, 3 ],[ "TAY", self.TAY, self.IMP, 2 ],[ "LDA", self.LDA, self.IMM, 2 ],[ "TAX", self.TAX, self.IMP, 2 ],[ "???", self.XXX, self.IMP, 2 ],[ "LDY", self.LDY, self.ABS, 4 ],[ "LDA", self.LDA, self.ABS, 4 ],[ "LDX", self.LDX, self.ABS, 4 ],[ "???", self.XXX, self.IMP, 4 ],
                      [ "BCS", self.BCS, self.REL, 2 ],[ "LDA", self.LDA, self.IZY, 5 ],[ "???", self.XXX, self.IMP, 2 ],[ "???", self.XXX, self.IMP, 5 ],[ "LDY", self.LDY, self.ZPX, 4 ],[ "LDA", self.LDA, self.ZPX, 4 ],[ "LDX", self.LDX, self.ZPY, 4 ],[ "???", self.XXX, self.IMP, 4 ],[ "CLV", self.CLV, self.IMP, 2 ],[ "LDA", self.LDA, self.ABY, 4 ],[ "TSX", self.TSX, self.IMP, 2 ],[ "???", self.XXX, self.IMP, 4 ],[ "LDY", self.LDY, self.ABX, 4 ],[ "LDA", self.LDA, self.ABX, 4 ],[ "LDX", self.LDX, self.ABY, 4 ],[ "???", self.XXX, self.IMP, 4 ],
                      [ "CPY", self.CPY, self.IMM, 2 ],[ "CMP", self.CMP, self.IZX, 6 ],[ "???", self.NOP, self.IMP, 2 ],[ "???", self.XXX, self.IMP, 8 ],[ "CPY", self.CPY, self.ZP0, 3 ],[ "CMP", self.CMP, self.ZP0, 3 ],[ "DEC", self.DEC, self.ZP0, 5 ],[ "???", self.XXX, self.IMP, 5 ],[ "INY", self.INY, self.IMP, 2 ],[ "CMP", self.CMP, self.IMM, 2 ],[ "DEX", self.DEX, self.IMP, 2 ],[ "???", self.XXX, self.IMP, 2 ],[ "CPY", self.CPY, self.ABS, 4 ],[ "CMP", self.CMP, self.ABS, 4 ],[ "DEC", self.DEC, self.ABS, 6 ],[ "???", self.XXX, self.IMP, 6 ],
                      [ "BNE", self.BNE, self.REL, 2 ],[ "CMP", self.CMP, self.IZY, 5 ],[ "???", self.XXX, self.IMP, 2 ],[ "???", self.XXX, self.IMP, 8 ],[ "???", self.NOP, self.IMP, 4 ],[ "CMP", self.CMP, self.ZPX, 4 ],[ "DEC", self.DEC, self.ZPX, 6 ],[ "???", self.XXX, self.IMP, 6 ],[ "CLD", self.CLD, self.IMP, 2 ],[ "CMP", self.CMP, self.ABY, 4 ],[ "NOP", self.NOP, self.IMP, 2 ],[ "???", self.XXX, self.IMP, 7 ],[ "???", self.NOP, self.IMP, 4 ],[ "CMP", self.CMP, self.ABX, 4 ],[ "DEC", self.DEC, self.ABX, 7 ],[ "???", self.XXX, self.IMP, 7 ],
                      [ "CPX", self.CPX, self.IMM, 2 ],[ "SBC", self.SBC, self.IZX, 6 ],[ "???", self.NOP, self.IMP, 2 ],[ "???", self.XXX, self.IMP, 8 ],[ "CPX", self.CPX, self.ZP0, 3 ],[ "SBC", self.SBC, self.ZP0, 3 ],[ "INC", self.INC, self.ZP0, 5 ],[ "???", self.XXX, self.IMP, 5 ],[ "INX", self.INX, self.IMP, 2 ],[ "SBC", self.SBC, self.IMM, 2 ],[ "NOP", self.NOP, self.IMP, 2 ],[ "???", self.SBC, self.IMP, 2 ],[ "CPX", self.CPX, self.ABS, 4 ],[ "SBC", self.SBC, self.ABS, 4 ],[ "INC", self.INC, self.ABS, 6 ],[ "???", self.XXX, self.IMP, 6 ],
                      [ "BEQ", self.BEQ, self.REL, 2 ],[ "SBC", self.SBC, self.IZY, 5 ],[ "???", self.XXX, self.IMP, 2 ],[ "???", self.XXX, self.IMP, 8 ],[ "???", self.NOP, self.IMP, 4 ],[ "SBC", self.SBC, self.ZPX, 4 ],[ "INC", self.INC, self.ZPX, 6 ],[ "???", self.XXX, self.IMP, 6 ],[ "SED", self.SED, self.IMP, 2 ],[ "SBC", self.SBC, self.ABY, 4 ],[ "NOP", self.NOP, self.IMP, 2 ],[ "???", self.XXX, self.IMP, 7 ],[ "???", self.NOP, self.IMP, 4 ],[ "SBC", self.SBC, self.ABX, 4 ],[ "INC", self.INC, self.ABX, 7 ],[ "???", self.XXX, self.IMP, 7 ]]

    def connect(self, bus):
        self.bus = bus

    def write(self, address, data):
        self.bus.write(address, data)

    def read(self, address):
        return self.bus.read(address)

    def getFlag(self, flag):
        return self.status & self.FLAGS6502[flag]

    def setFlag(self, flag, value):
        if value:
            self.status |= self.getFlag(flag)
        else:
            self.status &= ~self.getFlag(flag)

    # Addressing Modes
    def IMP(self):
        self.fetched = self.A
        return 0

    def IMM(self):
        self.PC += 1
        self.addr_abs = self.PC
        return 0

    def ZP0(self):
        self.addr_abs = self.read(self.PC)
        self.PC += 1
        self.addr_abs &= 0x00FF
        return 0

    def ZPX(self):
        self.addr_abs = (self.read(self.PC) + self.X) & 0x00FF
        self.PC += 1

    def ZPY(self):
        self.addr_abs = (self.read(self.PC) + self.Y) & 0x00FF
        self.PC += 1

    def REL(self):
        self.addr_rel = self.read(self.PC)
        self.PC += 1
        if self.addr_rel & 0x80:
            self.addr_rel |= 0xFF00
        return 0

    def ABS(self):
        lo = self.read(self.PC)
        self.PC += 1
        hi = self.read(self.PC)
        self.PC += 1
        self.addr_abs = (hi << 8) | lo
        return 0

    def ABX(self):
        lo = self.read(self.PC)
        self.PC += 1
        hi = self.read(self.PC)
        self.PC += 1
        self.addr_abs = (hi << 8) | lo
        self.addr_abs += self.X

        if self.addr_abs & 0xFF00 != hi << 8:
            return 1
        else:
            return 0

    def ABY(self):
        lo = self.read(self.PC)
        self.PC += 1
        hi = self.read(self.PC)
        self.PC += 1
        self.addr_abs = (hi << 8) | lo
        self.addr_abs += self.Y

        if self.addr_abs & 0xFF00 != hi << 8:
            return 1
        else:
            return 0

    def IND(self):
        ptr_lo = self.read(self.PC)
        self.PC += 1
        ptr_hi = self.read(self.PC)
        self.PC += 1
        ptr = (ptr_hi << 8) | ptr_lo
        self.addr_abs = (self.read(ptr + 1) << 8) | self.read(ptr)
        return 0

    def IZX(self):
        t = self.read(self.PC)
        self.PC += 1

        lo = self.read((t + self.X) & 0x00FF)
        hi = self.read((t + self.X + 1) & 0x00FF)
        self.addr_abs = (hi << 8) | lo

        return 0

    def IZY(self):
        t = self.read(self.PC)
        self.PC += 1

        lo = self.read((t + self.Y) & 0x00FF)
        hi = self.read((t + self.Y + 1) & 0x00FF)
        self.addr_abs = (hi << 8) | lo

        return 0

    # Fetching the data
    def fetch(self):
        if INSTRUCTION(self.codes[self.opcode]).address_mode != self.IMP:
            self.fetched = self.read(self.addr_abs)
        return self.fetched

    # Opcodes
    def ADC(self):
        self.fetch()
        temp = self.A + self.fetched + self.getFlag("C")
        self.setFlag("C", temp > 255)
        self.setFlag("Z", temp & 0x00FF == 0)
        self.setFlag("N", temp & 0x80)
        self.setFlag("V", ((self.A ^ self.fetched) & 0x80) and ((self.A ^ temp) & 0x0080))
        self.A = temp & 0x00FF
        return 1

    def AND(self):
        self.fetch()
        self.A &= self.fetched
        self.setFlag("Z", self.A == 0)
        self.setFlag("N", self.A & 0x0080)
        return 1

    def ASL(self):
        self.fetch()
        temp = self.fetched << 1
        self.setFlag("C", temp & 0xFF00 > 0)
        self.setFlag("Z", temp & 0x00FF == 0)
        self.setFlag("N", temp & 0x80)

        if INSTRUCTION(self.codes[self.opcode]).address_mode == self.IMP:
            self.A = temp & 0x00FF
        else:
            self.write(self.addr_abs, temp & 0x00FF)
        return 0

    def BCC(self):
        if self.getFlag("C") == 0:
            self.cycles += 1
            self.addr_abs = self.PC + self.addr_rel

            if (self.addr_abs & 0xFF00) != (self.PC & 0xFF00):
                self.cycles += 1

            self.PC = self.addr_abs

        return 0

    def BCS(self):
        if self.getFlag("C") == 1:
            self.cycles += 1
            self.addr_abs = self.PC + self.addr_rel

            if (self.addr_abs & 0xFF00) != (self.PC & 0xFF00):
                self.cycles += 1

            self.PC = self.addr_abs

        return 0

    def BEQ(self):
        if self.getFlag("Z") == 1:
            self.cycles += 1
            self.addr_abs = self.PC + self.addr_rel

            if (self.addr_abs & 0xFF00) != (self.PC & 0xFF00):
                self.cycles += 1

            self.PC = self.addr_abs

        return 0

    def BIT(self):
        self.fetch()
        temp = self.A & self.fetched
        self.setFlag("Z", temp & 0x00FF == 0)
        self.setFlag("N", self.fetched & (1 << 7))
        self.setFlag("V", self.fetched & (1 << 6))
        return 0

    def BMI(self):
        if self.getFlag("N") == 1:
            self.cycles += 1
            self.addr_abs = self.PC + self.addr_rel

            if (self.addr_abs & 0xFF00) != (self.PC & 0xFF00):
                self.cycles += 1

            self.PC = self.addr_abs

        return 0

    def BNE(self):
        if self.getFlag("Z") == 0:
            self.cycles += 1
            self.addr_abs = self.PC + self.addr_rel

            if (self.addr_abs & 0xFF00) != (self.PC & 0xFF00):
                self.cycles += 1

            self.PC = self.addr_abs
        return 0

    def BPL(self):
        if self.getFlag("N") == 0:
            self.cycles += 1
            self.addr_abs = self.PC + self.addr_rel

            if (self.addr_abs & 0xFF00) != (self.PC & 0xFF00):
                self.cycles += 1

            self.PC = self.addr_abs

        return 0

    def BRK(self):
        self.PC += 1
        self.setFlag("I", 1)
        self.write(0x0100 + self.STKP, (self.PC >> 8) & 0x00FF)
        self.STKP -= 1
        self.write(0x0100 + self.STKP, self.PC & 0x00FF)
        self.STKP -= 1

        self.setFlag("B", 1)
        self.write(0x0100 + self.STKP, self.status)
        self.STKP -= 1

        self.setFlag("B", 0)
        self.PC = self.read(0xFFFE) | (self.read(0xFFFF) << 8)
        return 0

    def BVC(self):
        if self.getFlag("V") == 1:
            self.cycles += 1
            self.addr_abs = self.PC + self.addr_rel

            if (self.addr_abs & 0xFF00) != (self.PC & 0xFF00):
                self.cycles += 1

            self.PC = self.addr_abs

        return 0

    def BVS(self):
        if self.getFlag("V") == 1:
            self.cycles += 1
            self.addr_abs = self.PC + self.addr_rel

            if (self.addr_abs & 0xFF00) != (self.PC & 0xFF00):
                self.cycles += 1

            self.PC = self.addr_abs
        return 0

    def CLC(self):
        self.setFlag("C", 0)
        return 0

    def CLD(self):
        self.setFlag("D", 0)
        return 0

    def CLI(self):
        self.setFlag("I", 0)
        return 0

    def CLV(self):
        self.setFlag("V", 0)
        return 0

    def CMP(self):
        self.fetch()
        temp = self.A - self.fetched
        self.setFlag("C", temp >= 0)
        self.setFlag("Z", temp & 0x00FF == 0)
        self.setFlag("N", temp & 0x80)
        return 1

    def CPX(self):
        self.fetch()
        temp = self.X - self.fetched
        self.setFlag("C", temp >= 0)
        self.setFlag("Z", temp & 0x00FF == 0)
        self.setFlag("N", temp & 0x80)
        return 1

    def CPY(self):
        self.fetch()
        temp = self.Y - self.fetched
        self.setFlag("C", temp >= 0)
        self.setFlag("Z", temp & 0x00FF == 0)
        self.setFlag("N", temp & 0x80)
        return 1

    def DEC(self):
        self.fetch()
        temp = self.fetched - 1
        self.setFlag("Z", temp & 0x00FF == 0)
        self.setFlag("N", temp & 0x80)
        return 0

    def DEX(self):
        self.X -= 1
        self.setFlag("Z", self.X == 0)
        self.setFlag("N", self.X & 0x80)
        return 0

    def DEY(self):
        self.Y -= 1
        self.setFlag("Z", self.Y == 0)
        self.setFlag("N", self.Y & 0x80)
        return 0

    def EOR(self):
        self.fetch()
        self.A = self.A ^ self.fetched
        self.setFlag("Z", self.A == 0)
        self.setFlag("N", self.A & 0x80)
        return 1

    def INC(self):
        self.fetch()
        temp = self.fetched + 1
        self.write(self.addr_abs, temp & 0x00FF)
        self.setFlag("Z", temp & 0x00FF == 0)
        self.setFlag("N", temp & 0x80)
        return 0

    def INX(self):
        self.X += 1
        self.setFlag("Z", self.X == 0)
        self.setFlag("N", self.X & 0x80)
        return 0

    def INY(self):
        self.Y += 1
        self.setFlag("Z", self.Y == 0)
        self.setFlag("N", self.Y & 0x80)
        return 0

    def JMP(self):
        self.PC = self.addr_abs
        return 0

    def JSR(self):
        self.PC -= 1
        self.write(0x0100 + self.STKP, (self.PC >> 8) & 0x00FF)
        self.STKP -= 1
        self.write(0x0100 + self.STKP, self.PC & 0x00FF)
        self.STKP -= 1
        self.PC = self.addr_abs
        return 0

    def LDA(self):
        self.fetch()
        self.A = self.fetched
        self.setFlag("Z", self.A == 0)
        self.setFlag("N", self.A & 0x80)
        return 1

    def LDX(self):
        self.fetch()
        self.X = self.fetched
        self.setFlag("Z", self.X == 0)
        self.setFlag("N", self.X & 0x80)
        return 1

    def LDY(self):
        self.fetch()
        self.Y = self.fetched
        self.setFlag("Z", self.Y == 0)
        self.setFlag("N", self.Y & 0x80)
        return 1

    def LSR(self):
        self.fetch()
        self.setFlag("C", self.fetched & 0x0001)
        temp = self.fetched >> 1
        self.setFlag("Z", temp & 0x00FF == 0)
        self.setFlag("N", temp & 0x80)
        if INSTRUCTION(self.codes[self.opcode]).address_mode == self.IMP:
            self.A = temp & 0x00FF
        else:
            self.write(self.addr_abs, temp & 0x00FF)
        return 0

    def NOP(self):
        return 0

    def ORA(self):
        self.fetch()
        self.A = self.A | self.fetched
        self.setFlag("Z", self.A == 0)
        self.setFlag("N", self.A & 0x80)
        return 1

    def PHA(self):
        self.write(0x100 + self.STKP, self.A)
        self.STKP -= 1
        return 0

    def PHP(self):
        self.write(0x100 + self.STKP, self.status | self.FLAGS6502["B"] | self.FLAGS6502["U"])
        self.STKP -= 1
        self.setFlag("B", 0)
        self.setFlag("U", 1)
        return 0

    def PLA(self):
        self.STKP += 1
        self.A = self.read(0x100 + self.STKP)
        self.setFlag("Z", self.A == 0)
        self.setFlag("N", self.A & 0x80)
        return 0

    def PLP(self):
        self.STKP += 1
        self.status = self.read(0x100 + self.STKP)
        self.setFlag("U", 1)
        return 0

    def ROL(self):
        self.fetch()
        temp = self.fetched << 1 | self.getFlag("C")
        self.setFlag("C", self.fetched & 0xFF00)
        self.setFlag("Z", temp & 0x00FF == 0x0000)
        self.setFlag("N", temp & 0x80)
        if INSTRUCTION(self.codes[self.opcode]).address_mode == self.IMP:
            self.A = temp & 0x00FF
        else:
            self.write(self.addr_abs, temp & 0x00FF)

        return 0

    def ROR(self):
        self.fetch()
        temp = (self.getFlag("C") << 7) | self.fetched >> 1
        self.setFlag("C", self.fetched & 0x01)
        self.setFlag("Z", temp & 0x00FF == 0x0000)
        self.setFlag("N", temp & 0x80)
        if INSTRUCTION(self.codes[self.opcode]).address_mode == self.IMP:
            self.A = temp & 0x00FF
        else:
            self.write(self.addr_abs, temp & 0x00FF)
        return 0

    def RTI(self):
        self.STKP += 1
        self.status = self.read(0x100 + self.STKP)
        self.status &= ~self.FLAGS6502["B"]
        self.status &= ~self.FLAGS6502["U"]

        self.STKP += 1
        self.PC = self.read(0x100 + self.STKP)
        self.STKP += 1
        self.PC |= self.read(0x100 + self.STKP) << 8
        return 0

    def RTS(self):
        self.STKP += 1
        self.PC = self.read(0x100 + self.STKP)
        self.STKP += 1
        self.PC |= self.read(0x100 + self.STKP) << 8
        self.PC += 1
        return 0

    def SBC(self):
        self.fetch()
        value = self.fetched ^ 0x00FF

        temp = self.A + value + self.getFlag("C")
        self.setFlag("C", temp & 0xFF00)
        self.setFlag("Z", temp & 0x00FF == 0)
        self.setFlag("V", (temp ^ self.A) & (temp ^ value) & 0x0080)
        self.setFlag("N", temp & 0x80)
        self.A = temp & 0x00FF
        return 1

    def SEC(self):
        self.setFlag("C", 1)
        return 0

    def SED(self):
        self.setFlag("D", 1)
        return 0

    def SEI(self):
        self.setFlag("I", 1)
        return 0

    def STA(self):
        self.write(self.addr_abs, self.A)
        return 0

    def STX(self):
        self.write(self.addr_abs, self.X)
        return 0

    def STY(self):
        self.write(self.addr_abs, self.Y)
        return 0

    def TAX(self):
        self.X = self.A
        self.setFlag("Z", self.X == 0)
        self.setFlag("N", self.X & 0x80)
        return 0

    def TAY(self):
        self.Y = self.A
        self.setFlag("Z", self.Y == 0)
        self.setFlag("N", self.Y & 0x80)
        return 0

    def TSX(self):
        self.X = self.STKP
        self.setFlag("Z", self.X == 0)
        self.setFlag("N", self.X & 0x80)
        return 0

    def TXA(self):
        self.A = self.X
        self.setFlag("Z", self.A == 0)
        self.setFlag("N", self.A & 0x80)
        return 0

    def TXS(self):
        self.STKP = self.X
        return 0

    def TYA(self):
        self.A = self.Y
        self.setFlag("Z", self.A == 0)
        self.setFlag("N", self.A & 0x80)
        return 0

    def XXX(self):
        return 0

    # Interrupts
    def reset(self):
        self.A = 0
        self.X = 0
        self.Y = 0
        self.STKP = 0xFD
        self.status = 0x00 | self.FLAGS6502["U"]

        self.addr_abs = 0xFFC
        lo = self.read(self.addr_abs)
        hi = self.read(self.addr_abs + 1)

        self.PC = (hi << 8) | lo

        self.addr_rel = 0
        self.addr_abs = 0
        self.fetched = 0

        self.cycles = 8

    def irq(self):
        if self.getFlag("I") == 0:

            self.write(0x100 + self.STKP, (self.PC >> 8) & 0x00FF)
            self.STKP -= 1
            self.write(0x100 + self.STKP, self.PC & 0x00FF)
            self.STKP -= 1

            self.setFlag("B", 0)
            self.setFlag("U", 1)
            self.setFlag("I", 1)
            self.write(0x100 + self.STKP, self.status)
            self.STKP -= 1

            addr_abs = 0xFFFE
            lo = self.read(addr_abs)
            hi = self.read(addr_abs + 1)
            self.PC = (hi << 8) | lo

            self.cycles = 7

    def nmi(self):
        self.write(0x100 + self.STKP, (self.PC >> 8) & 0x00FF)
        self.STKP -= 1
        self.write(0x100 + self.STKP, self.PC & 0x00FF)
        self.STKP -= 1

        self.setFlag("B", 0)
        self.setFlag("U", 1)
        self.setFlag("I", 1)
        self.write(0x100 + self.STKP, self.status)
        self.STKP -= 1

        addr_abs = 0xFFFA
        lo = self.read(addr_abs)
        hi = self.read(addr_abs + 1)
        self.PC = (hi << 8) | lo

        self.cycles = 8

    """
    #############################################################################
                        DISPLAY FUNCTION
    #############################################################################
    """
    def display(self):
        print("\n")
        # Local registers
        print("PC: " + hex(self.PC))
        print("A: " + hex(self.A))
        print("X: " + hex(self.X))
        print("Y: " + hex(self.Y))

        # Ram overview
        print(self.bus.ram[0x0000:0x00F0])
        print("\n")
        print(self.bus.ram[0x8000:0x80F0])
        input("Press Enter...")

    # Clock
    def clock(self):
        self.display()
        if self.cycles == 0:
            self.opcode = self.read(self.PC)
            self.PC += 1
            instruction = INSTRUCTION(self.codes[self.opcode])
            self.cycles = instruction.cycles

            additionnal_cycle1 = instruction.address_mode(self)
            additionnal_cycle2 = instruction.operate(self)

            self.cycles += (additionnal_cycle1 & additionnal_cycle2)

        self.cycles -= 1

    # Helper functions
    def load(self, programm):
        offset = 0x8000
        for i in range(len(programm)):
            self.write(offset + i, programm[i])

        self.write(0xFFC, 0x00)
        self.write(0xFFD, 0x80)

        self.reset()
        return True

