defmodule AdventOfCode.Y2019.Day01Test do
  @moduledoc false
  use ExUnit.Case
  @moduletag :y1901

  alias AdventOfCode.Y2019.Day01, as: Solution

  test "Year 2019, Day 1, Part 1" do
    assert Solution.run_1() == 3_421_505
  end

  test "Year 2019, Day 1, Part 2" do
    assert Solution.run_2() == 5_129_386
  end
end
