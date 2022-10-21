defmodule AdventOfCode.Y2016.Day09Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y1609

  alias AdventOfCode.Y2016.Day09, as: Solution

  describe "Year 2016, Day 9, Part 1" do
    params = [
      {"ADVENT", 6},
      {"A(1x5)BC", 7},
      {"(3x3)XYZ", 9},
      {"A(2x2)BCD(2x2)EFG", 11},
      {"(6x1)(1x3)A", 6},
      {"X(8x2)(3x3)ABCY", 18}
    ]

    for {input, output} <- params do
      test "Works for #{input}" do
        assert Solution.run_1(unquote(input)) == unquote(output)
      end
    end

    test "Result" do
      assert Solution.run_1(Solution.input!()) == 102_239
    end
  end

  describe "Year 2016, Day 9, Part 2" do
    params = [
      {"ADVENT", 6},
      {"(3x3)XYZ", 9},
      {"X(8x2)(3x3)ABCY", 20},
      {"(27x12)(20x12)(13x14)(7x10)(1x12)A", 241_920},
      {"(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN", 445},
    ]

    for {input, output} <- params do
      test "Works for #{input}" do
        assert Solution.run_2(unquote(input)) == unquote(output)
      end
    end

    test "Result" do
      assert Solution.run_2(Solution.input!()) == 10_780_403_063
    end
  end
end
