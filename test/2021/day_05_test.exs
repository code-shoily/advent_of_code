defmodule AdventOfCode.Y2021.Day05Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2105

  alias AdventOfCode.Y2021.Day05, as: Solution

  test "Year 2021, Day 5, Part 1" do
    assert Solution.run_1() == 4655
  end

  test "Year 2021, Day 5, Part 2" do
    assert Solution.run_2() == 20_500
  end
end
