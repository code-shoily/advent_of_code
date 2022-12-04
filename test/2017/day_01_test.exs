defmodule AdventOfCode.Y2017.Day01Test do
  @moduledoc false
  use ExUnit.Case
  @moduletag :y1701

  alias AdventOfCode.Y2017.Day01, as: Solution

  test "Year 2017, Day 1" do
    assert Solution.run() == {1089, 1156}
  end
end
