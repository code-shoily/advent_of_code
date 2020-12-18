defmodule AdventOfCode.Y2020.Day18Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2018

  alias AdventOfCode.Y2020.Day18, as: Solution

  test "Year 2020, Day 18, Part 1" do
    assert Solution.run_1() == 50_956_598_240_016
  end

  test "Year 2020, Day 18, Part 2" do
    assert Solution.run_2() == 535_809_575_344_339
  end
end
