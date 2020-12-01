defmodule AdventOfCode.Y2020.Day1Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y201

  alias AdventOfCode.Y2020.Day1, as: Solution

  test "Year 2020, Day 1, Part 1" do
    assert Solution.run_1() == 1_014_624
  end

  test "Year 2020, Day 1, Part 2" do
    assert Solution.run_2() == 80_072_256
  end
end
