use core::fmt;
use std::{fs::File, io::Read};

pub const NES_TAG: [u8; 4] = [0x4E, 0x45, 0x53, 0x1A];
pub const PRG_ROM_PAGE_SIZE: usize = 16384;
pub const CHR_ROM_PAGE_SIZE: usize = 8192;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Mirroring {
    Vertical,
    Horizontal,
    FourScreen,
}

impl fmt::Display for Mirroring {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mirroring = match self {
            Mirroring::Vertical => "VERTICAL",
            Mirroring::Horizontal => "HORIZONATAL",
            Mirroring::FourScreen => "FOUR_SCREEN",
        };

        write!(f, "{}", mirroring)
    }
}

pub struct Rom {
    pub prg_rom: Vec<u8>,
    pub chr_rom: Vec<u8>,
    pub mapper: u8,
    pub screen_mirroring: Mirroring,
    allow_chr_writes: bool,
}

impl fmt::Display for Rom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "\nprg_rom: {:?}, \nchr_rom: {:?}, \nmapper: {:#04x}, \nmirroring: {}",
            self.prg_rom, self.chr_rom, self.mapper, self.screen_mirroring
        )
    }
}

impl Rom {
    pub fn new(raw: &Vec<u8>) -> Result<Rom, String> {
        if raw.len() < 8 {
            return Err("File is not in iNES file format".to_string());
        }

        if &raw[0..4] != NES_TAG {
            return Err("File is not in iNES file format".to_string());
        }

        let mapper = (raw[7] & 0b1111_0000) | (raw[6] >> 4);

        let ines_ver = (raw[7] >> 2) & 0b11;
        if ines_ver != 0 {
            return Err("NES2.0 is not supported".to_string());
        }

        let four_screen = raw[6] & 0b1000 != 0;
        let vertical_mirroring = raw[6] & 0b1 != 0;
        let screen_mirroring = match (four_screen, vertical_mirroring) {
            (true, _) => Mirroring::FourScreen,
            (false, true) => Mirroring::Vertical,
            (false, false) => Mirroring::Horizontal,
        };

        let prg_rom_size = raw[4] as usize * PRG_ROM_PAGE_SIZE;
        let chr_rom_size = raw[5] as usize * CHR_ROM_PAGE_SIZE;

        let skip_trainer = raw[6] & 0b100 != 0;

        let prg_rom_start = 16 + if skip_trainer { 512 } else { 0 };
        let chr_rom_start = prg_rom_start + prg_rom_size;

        let allow_chr_rom_writes = chr_rom_size == 0;
        let chr_rom = if chr_rom_size > 0 {
            raw[chr_rom_start..(chr_rom_start + chr_rom_size)].to_vec()
        } else {
            vec![0; 8 * 1024] //chr_ram
        };

        Ok(Rom {
            prg_rom: raw[prg_rom_start..(prg_rom_start + prg_rom_size)].to_vec(),
            chr_rom: chr_rom,
            mapper: mapper,
            screen_mirroring: screen_mirroring,
            allow_chr_writes: allow_chr_rom_writes,
        })
    }

    pub fn from_file(file_path: &str) -> Rom {
        let mut file = match File::open(file_path) {
            Ok(f) => f,
            Err(e) => panic!("Could not open file {}", e),
        };

        let mut buffer = Vec::new();
        match file.read_to_end(&mut buffer) {
            Ok(_) => (),
            Err(e) => panic!("Could not read file contents {}", e),
        };

        match Rom::new(&buffer) {
            Ok(r) => r,
            Err(s) => panic!("{}", s),
        }
    }

    pub fn write_to_chr_rom(&mut self, addr: usize, data: u8) -> u8 {
        if !self.allow_chr_writes {
            panic!("Attempt to write to chr rom space {:#04X}", addr);
        }

        let retval = self.chr_rom[addr];
        self.chr_rom[addr] = data;
        retval
    }
}

#[cfg(test)]
pub mod test {
    use super::*;

    struct TestRom {
        header: Vec<u8>,
        trainer: Option<Vec<u8>>,
        pgp_rom: Vec<u8>,
        chr_rom: Vec<u8>,
    }

    fn create_rom(rom: TestRom) -> Vec<u8> {
        let mut result = Vec::with_capacity(
            rom.header.len()
                + rom.trainer.as_ref().map_or(0, |t| t.len())
                + rom.pgp_rom.len()
                + rom.chr_rom.len(),
        );

        result.extend(&rom.header);
        if let Some(t) = rom.trainer {
            result.extend(t);
        }
        result.extend(&rom.pgp_rom);
        result.extend(&rom.chr_rom);

        result
    }

    pub fn test_rom(program: Vec<u8>) -> Rom {
        let mut pgp_rom_contents = program;
        pgp_rom_contents.resize(2 * PRG_ROM_PAGE_SIZE, 0);

        let test_rom = create_rom(TestRom {
            header: vec![
                0x4E, 0x45, 0x53, 0x1A, 0x02, 0x01, 0x31, 00, 00, 00, 00, 00, 00, 00, 00, 00,
            ],
            trainer: None,
            pgp_rom: pgp_rom_contents,
            chr_rom: vec![2; 1 * CHR_ROM_PAGE_SIZE],
        });

        Rom::new(&test_rom).unwrap()
    }

    #[test]
    fn test_invalid_file_format_too_short() {
        let rom = Rom::new(&vec![0x00]);
        match rom {
            Err(ref e) => assert_eq!("File is not in iNES file format", e),
            Ok(_) => panic!("Expected failure, got success"),
        }
    }

    #[test]
    fn test_invalid_file_format_invalid_nes_tag() {
        let rom = Rom::new(&vec![0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]);
        match rom {
            Err(ref e) => assert_eq!("File is not in iNES file format", e),
            Ok(_) => panic!("Expected failure, got success"),
        }
    }

    #[test]
    fn test_invalid_file_format_unsupported_version() {
        let rom = Rom::new(&vec![0x4E, 0x45, 0x53, 0x1A, 0x00, 0x00, 0x00, 0x0C]);
        match rom {
            Err(ref e) => assert_eq!("NES2.0 is not supported", e),
            Ok(_) => panic!("Expected failure, got success"),
        }
    }

    #[test]
    fn test_valid_rom_no_skip_trainer() {
        let mut prg_rom = vec![0x12, 0xAB, 0x6C, 0x11];
        prg_rom.resize(PRG_ROM_PAGE_SIZE, 0x00);
        let mut chr_rom = vec![0x43, 0xFF, 0xFA, 0xAB];
        chr_rom.resize(2 * CHR_ROM_PAGE_SIZE, 0x00);

        //First few elements of raw, specifying iNES version and rom sizes
        let mut raw = vec![0x4E, 0x45, 0x53, 0x1A, 0x01, 0x02, 0b0000_1000, 0x00];
        raw.resize(16 + PRG_ROM_PAGE_SIZE + 2 * CHR_ROM_PAGE_SIZE, 0x00);

        //Copy our rom over
        raw[16..16 + PRG_ROM_PAGE_SIZE].copy_from_slice(&prg_rom);
        raw[16 + PRG_ROM_PAGE_SIZE..16 + PRG_ROM_PAGE_SIZE + 2 * CHR_ROM_PAGE_SIZE]
            .copy_from_slice(&chr_rom);

        let maybe_rom = Rom::new(&raw);

        let rom = match maybe_rom {
            Err(_) => panic!("Expected success, got error"),
            Ok(r) => r,
        };

        assert_eq!(Mirroring::FourScreen, rom.screen_mirroring);
        assert_eq!(prg_rom, rom.prg_rom);
        assert_eq!(chr_rom, rom.chr_rom);
    }

    #[test]
    fn test_valid_rom_skip_trainer() {
        let mut prg_rom = vec![0x12, 0xAB, 0x6C, 0x11];
        prg_rom.resize(PRG_ROM_PAGE_SIZE, 0x00);
        let mut chr_rom = vec![0x43, 0xFF, 0xFA, 0xAB];
        chr_rom.resize(2 * CHR_ROM_PAGE_SIZE, 0x00);

        //First few elements of raw, specifying iNES version and rom sizes
        let mut raw = vec![0x4E, 0x45, 0x53, 0x1A, 0x01, 0x02, 0b0000_0101, 0x00];
        raw.resize(16 + 512 + PRG_ROM_PAGE_SIZE + 2 * CHR_ROM_PAGE_SIZE, 0x00);

        //Copy our rom over
        raw[16 + 512..16 + 512 + PRG_ROM_PAGE_SIZE].copy_from_slice(&prg_rom);
        raw[16 + 512 + PRG_ROM_PAGE_SIZE..16 + 512 + PRG_ROM_PAGE_SIZE + 2 * CHR_ROM_PAGE_SIZE]
            .copy_from_slice(&chr_rom);

        let maybe_rom = Rom::new(&raw);

        let rom = match maybe_rom {
            Err(_) => panic!("Expected success, got error"),
            Ok(r) => r,
        };

        assert_eq!(prg_rom, rom.prg_rom);
        assert_eq!(chr_rom, rom.chr_rom);
        assert_eq!(Mirroring::Vertical, rom.screen_mirroring);
    }
}
