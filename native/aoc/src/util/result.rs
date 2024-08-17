use rustler::NifTaggedEnum;


#[derive(NifTaggedEnum)]
pub enum Solution {
  BothString((String, String)),
  BothInt32((i32, i32)),
  BothUSize((usize, usize)),
}