pub struct LenCounter {
    len_counter: u8,
    disabled: bool,
}

pub const LENGTH_TABLE: [u8; 32] = [
    10,  //0 0000
    254, //0 0001
    20,  //0 0010
    2,   //0 0011
    40,  //0 0100
    4,   //0 0101
    80,  //0 0110
    6,   //0 0111
    160, //0 1000
    8,   //0 1001
    60,  //0 1010
    10,  //0 1011
    14,  //0 1100
    12,  //0 1101
    26,  //0 1110
    14,  //0 1111
    12,  //1 0000
    16,  //1 0001
    24,  //1 0010
    18,  //1 0011
    48,  //1 0100
    20,  //1 0101
    96,  //1 0110
    22,  //1 0111
    192, //1 1000
    24,  //1 1001
    72,  //1 1010
    26,  //1 1011
    16,  //1 1100
    28,  //1 1101
    32,  //1 1110
    30,  //1 1111
];

impl LenCounter {
    pub fn new() -> Self {
        LenCounter {
            len_counter: 0,
            disabled: false,
        }
    }

    pub fn get(&self) -> u8 {
        self.len_counter
    }

    pub fn decrement(&mut self) {
        self.len_counter = self.len_counter - 1;
    }

    pub fn set(&mut self, len_indx: usize) {
        if self.disabled {
            return;
        }
        self.len_counter = LENGTH_TABLE[len_indx];
    }

    pub fn disable(&mut self) {
        self.len_counter = 0;
        self.disabled = true;
    }

    pub fn enable(&mut self) {
        self.disabled = false;
    }
}
