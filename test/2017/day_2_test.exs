defmodule AdventOfCode.Y2017.Day2Test do
  @moduledoc false
  use ExUnit.Case
  @moduletag :y172

  alias AdventOfCode.Y2017.Day2, as: Solution

  test "Year 2017, Day 2, Part 1" do
    assert Solution.run_1() == 32_020
  end

  test "Year 2017, Day 2, Part 2" do
    assert Solution.run_2() == 236
  end
end
