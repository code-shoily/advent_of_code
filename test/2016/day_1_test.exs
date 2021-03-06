defmodule AdventOfCode.Y2016.Day1Test do
  @moduledoc false
  use ExUnit.Case

  alias AdventOfCode.Y2016.Day1, as: Solution

  test "Year 2016, Day 1, Part 1" do
    assert Solution.run_1() == 253
  end

  test "Year 2016, Day 1, Part 2" do
    assert Solution.run_2() == 126
  end
end
