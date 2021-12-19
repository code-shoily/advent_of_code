defmodule AdventOfCode.Y2021.Day09Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2109

  alias AdventOfCode.Y2021.Day09, as: Solution

  test "Year 2021, Day 9, Part 1" do
    assert Solution.run_1() == 528
  end

  test "Year 2021, Day 9, Part 2" do
    assert Solution.run_2() == 920_448
  end
end
