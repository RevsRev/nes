pub trait Registers {
    fn get_register_a(&self) -> u8;
    fn set_register_a(&mut self, value: u8);

    fn get_register_x(&self) -> u8;
    fn set_register_x(&mut self, value: u8);

    fn get_register_y(&self) -> u8;
    fn set_register_y(&mut self, value: u8);

    fn get_status(&self) -> u8;
    fn set_status(&mut self, value: u8);

    fn get_program_counter(&self) -> u16;
    fn set_program_counter(&mut self, value: u16);

    fn get_stack_pointer(&self) -> u8;
    fn set_stack_pointer(&mut self, value: u8);
}
