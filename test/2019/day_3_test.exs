defmodule AdventOfCode.Y2019.Day3Test do
  @moduledoc false
  use ExUnit.Case

  alias AdventOfCode.Y2019.Day3, as: Solution

  test "Year 2019, Day 3, Part 1" do
    assert Solution.run_1() == 1195
  end

  test "Year 2019, Day 3, Part 2" do
    assert Solution.run_2() == 91_518
  end
end
