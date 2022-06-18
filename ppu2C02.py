from bus import *


class ppu2C02:

    def __init__(self):
        self.cartridge = None
        self.tblName = [np.zeros(1024, dtype=np.uint8), np.zeros(1024, dtype=np.uint8)]
        self.tblPalette = np.zeros(32, dtype=np.uint8)
        # self.tblPattern = [np.zeros(4096, dtype=np.uint8), np.zeros(4096, dtype=np.uint8)]

    def cpuRead(self, address):
        data = 0x00

        if address == 0x0000: pass  # Control
        elif address == 0x0001: pass  # Mask
        elif address == 0x0002: pass  # Status
        elif address == 0x0003: pass  # OAM Address
        elif address == 0x0004: pass  # OAM Data
        elif address == 0x0005: pass  # Scroll
        elif address == 0x0006: pass  # PPU Address
        elif address == 0x0007: pass  # PPU Data

        return data

    def cpuWrite(self, address, data):

        if address == 0x0000: pass  # Control
        elif address == 0x0001: pass  # Mask
        elif address == 0x0002: pass  # Status
        elif address == 0x0003: pass  # OAM Address
        elif address == 0x0004: pass  # OAM Data
        elif address == 0x0005: pass  # Scroll
        elif address == 0x0006: pass  # PPU Address
        elif address == 0x0007: pass  # PPU Data

    def ppuRead(self, address):
        data = 0x00
        address &= 0x3FFF
        return data

    def ppuWrite(self, address, data):
        address &= 0x3FFF

    def connectCartridge(self, cartridge):
        self.cartridge = cartridge

    def clock(self):
        pass