defmodule AdventOfCode.Y2020.Day7Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y207

  alias AdventOfCode.Y2020.Day7, as: Solution

  test "Year 2020, Day 7, Part 1" do
    assert Solution.run_1() == 355
  end

  test "Year 2020, Day 7, Part 2" do
    assert Solution.run_2() == 5312
  end
end
