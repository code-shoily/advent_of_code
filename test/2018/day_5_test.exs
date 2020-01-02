defmodule AdventOfCode.Y2018.Day5Test do
  @moduledoc false
  use ExUnit.Case

  alias AdventOfCode.Y2018.Day5, as: Solution

  test "Year 2018, Day 5, Part 1" do
    assert Solution.run_1() == 4284
  end

  test "Year 2018, Day 5, Part 2" do
    assert Solution.run_2() == 35490
  end
end
