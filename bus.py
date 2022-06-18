import numpy as np


# Bus
class Bus:
    def __init__(self):
        self.data = 0x00
        self.address = 0x0000
        self.ppu = None
        self.cpu = None
        self.cartridge = None

        self.nSystemClock = 0
        self.cpuRam = np.zeros(2048, dtype=np.uint8)

    # Main bus functions
    def cpuWrite(self, address, data):
        if 0x0000 <= address <= 0x1FFF:
            self.cpuRam[address & 0x07FF] = data

        elif 0x2000 <= address <= 0x3FFF:
            self.ppu.cpuWrite(address & 0x0007, data)

    def cpuRead(self, address, readOnly=False):

        data = 0x00
        if 0x0000 <= address <= 0x1FFF:
            data = self.cpuRam[address & 0x07FF]
        elif 0x2000 <= address <= 0x3FFF:
            data = self.ppu.cpuRead(address & 0x0007)

        return data

    # PPU bus functions
    def ppuWrite(self, address, data):
        pass

    def ppuRead(self, address, readOnly=False):
        pass

    # NES functions
    def insertCartridge(self, cartridge):
        self.cartridge = cartridge
        self.ppu.connectCartridge(cartridge)

    def reset(self):
        self.cpu.reset()
        nSystemClock = 0
   
