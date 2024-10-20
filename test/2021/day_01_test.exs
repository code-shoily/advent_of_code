defmodule AdventOfCode.Y2021.Day01Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2101

  alias AdventOfCode.Y2021.Day01, as: Solution

  test "Year 2021, Day 1" do
    assert Solution.run() == {1139, 1103}
  end
end
