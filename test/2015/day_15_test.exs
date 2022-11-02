defmodule AdventOfCode.Y2015.Day15Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y1515

  alias AdventOfCode.Y2015.Day15, as: Solution

  test "Year 2015, Day 15, Part 1" do
    assert Solution.run_1() == 13_882_464
  end

  test "Year 2015, Day 15, Part 2" do
    assert Solution.run_2() == 11_171_160
  end
end
