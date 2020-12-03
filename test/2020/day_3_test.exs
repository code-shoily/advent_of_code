defmodule AdventOfCode.Y2020.Day3Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y203

  alias AdventOfCode.Y2020.Day3, as: Solution

  test "Year 2020, Day 3, Part 1" do
    assert Solution.run_1() == 272
  end

  test "Year 2020, Day 3, Part 2" do
    assert Solution.run_2() == 3_898_725_600
  end
end
