defmodule AdventOfCode.Y2020.Day12Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2012

  alias AdventOfCode.Y2020.Day12, as: Solution

  test "Year 2020, Day 12, Part 1" do
    assert Solution.run_1() == 439
  end

  test "Year 2020, Day 12, Part 2" do
    assert Solution.run_2() == 12_385
  end
end
