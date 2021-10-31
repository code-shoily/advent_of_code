defmodule AdventOfCode.Y2017.Day09Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y1709

  alias AdventOfCode.Y2017.Day09, as: Solution

  test "Year 2017, Day 9, Part 1" do
    assert Solution.run_1() == 7_616
  end

  test "Year 2017, Day 9, Part 2" do
    assert Solution.run_2() == 3_838
  end
end
