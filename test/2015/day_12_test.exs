defmodule AdventOfCode.Y2015.Day12Test do
  @moduledoc false
  use ExUnit.Case

  alias AdventOfCode.Y2015.Day12, as: Solution

  test "Year 2015, Day 12, Part 1" do
    assert Solution.run_1() == 119_433
  end

  test "Year 2015, Day 12, Part 2" do
    assert Solution.run_2() == 68_466
  end
end
