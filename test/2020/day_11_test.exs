defmodule AdventOfCode.Y2020.Day11Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2011

  alias AdventOfCode.Y2020.Day11, as: Solution

  test "Year 2020, Day 11, Part 1" do
    assert Solution.run_1() == 2427
  end

  test "Year 2020, Day 11, Part 2" do
    assert Solution.run_2() == 2199
  end
end
