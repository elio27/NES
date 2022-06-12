import numpy as np
from instructions import *


# olc6502
class olc6502:
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
        #  self.addr_abs = self.PC + self.addr_rel  # NOT SURE ABOUT THIS ONE
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


    def fetch(self):
        if INSTRUCTION(self.opcode).address_mode != self.IMP:
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

        if INSTRUCTION(self.opcode).address_mode == self.IMP:
            a = temp & 0x00FF
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
        temp = a & self.fetched
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
        if INSTRUCTION(self.opcode).address_mode == self.IMP:
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
        temp = fetched << 1 | self.getFlag("C")
        self.setFlag("C", self.fetched & 0xFF00)
        self.setFlag("Z", temp & 0x00FF == 0x0000)
        self.setFlag("N", temp & 0x80)
        if INSTRUCTION(self.opcode).address_mode == self.IMP:
            self.A = temp & 0x00FF
        else:
            self.write(self.addr_abs, temp & 0x00FF)

        return 0

    def ROR(self):
        self.fetch()
        temp = (getFlag("C") << 7) | (self.fetched >> 1)

    def RTI(self):
        pass
    def RTS(self):
        pass

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
        pass
    def SED(self):
        pass
    def SEI(self):
        pass
    def STA(self):
        pass
    def STX(self):
        pass
    def STY(self):
        pass
    def TAX(self):
        pass
    def TAY(self):
        pass
    def TSX(self):
        pass
    def TXA(self):
        pass
    def TXS(self):
        pass
    def TYA(self):
        pass

    def XXX(self):
        pass

    def clock(self):
        if self.cycles == 0:
            self.opcode = self.read(self.PC)
            self.PC += 1
            instruction = INSTRUCTION(self.opcode)
            self.cycles = instruction.cycles

            additionnal_cycle1 = instruction.address_mode(self)
            additionnal_cycle2 = instruction.operate(self)

            self.cycles += (additionnal_cycle1 & additionnal_cycle2)

        self.cycles -= 1

    def reset(self):
        self.A = 0
        self.X = 0
        self.Y = 0
        self.STKP = 0xFD
        self.status = 0x00 | getFlag("U")

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

    def RTI(self):
        self.STKP += 1
        self.status = self.read(0x100 + self.STKP)
        self.status &= ~getFlag("B")
        self.status &= ~getFlag("U")

        self.STKP += 1
        self.PC = self.read(0x100 + self.STKP)
        self.STKP += 1
        self.PC |= self.read(0x100 + self.STKP) << 8
        return 0



# Bus
class Bus:
    def __init__(self, cpu):
        self.data = 0x00
        self.address = 0x0000

        self.cpu = cpu
        self.ram = np.zeros(0xFFFF, dtype=np.uint8)

    def write(self, address, data):
        if 0xFFFF > address >= 0:
            self.ram[address] = data
        return 0x00

    def read(self, address, readOnly=False):
        return self.ram[address]
