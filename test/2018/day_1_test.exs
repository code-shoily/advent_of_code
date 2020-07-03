defmodule AdventOfCode.Y2018.Day1Test do
  @moduledoc false
  use ExUnit.Case

  alias AdventOfCode.Y2018.Day1, as: Solution

  test "Year 2018, Day 1, Part 1" do
    assert Solution.run_1() == 590
  end

  test "Year 2018, Day 1, Part 2" do
    assert Solution.run_2() == 83_445
  end
end
