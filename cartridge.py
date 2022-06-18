from bus import *


# Follows the iNES format:
class Header:
    def __init__(self, data):
        self.name = data[0:3]
        self.prg_rom_chunks = data[4]
        self.chr_rom_chunks = data[5]
        self.mapper1 = data[6]
        self.mapper2 = data[7]
        self.prg_ram_size = data[8]
        self.tv_system1 = data[9]
        self.tv_system2 = data[10]
        self.unused = data[11:15]


class Cartridge:
    def __init__(self, filename):
        self.vPRGM = []
        self.vCHR = []

        self.nMapperID = 0
        self.nPRGBanks = 0
        self.nCHRBanks = 0

        with open(filename, 'rb') as f:
            header = Header(f.read(16))

            if header.mapper1 & 0x04:
                f.seek(512, 1)
