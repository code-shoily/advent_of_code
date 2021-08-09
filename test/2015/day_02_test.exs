defmodule AdventOfCode.Y2015.Day2Test do
  @moduledoc false
  use ExUnit.Case
  @moduletag :y1502

  alias AdventOfCode.Y2015.Day02, as: Solution

  test "Year 2015, Day 2, Part 1" do
    assert Solution.run_1() == 1_606_483
  end

  test "Year 2015, Day 2, Part 2" do
    assert Solution.run_2() == 3_842_356
  end
end
