defmodule AdventOfCode.Y2020.Day05Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2005

  alias AdventOfCode.Y2020.Day05, as: Solution

  test "Year 2020, Day 5, Part 1" do
    assert Solution.run_1() == 930
  end

  test "Year 2020, Day 5, Part 2" do
    assert Solution.run_2() == 515
  end
end
