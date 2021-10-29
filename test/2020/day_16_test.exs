defmodule AdventOfCode.Y2020.Day16Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2016

  alias AdventOfCode.Y2020.Day16, as: Solution

  test "Year 2020, Day 16, Part 1" do
    assert Solution.run_1() == 32_835
  end

  @tag :skip
  test "Year 2020, Day 16, Part 2" do
    assert Solution.run_2() == nil
  end
end
