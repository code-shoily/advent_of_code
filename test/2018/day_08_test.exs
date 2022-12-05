defmodule AdventOfCode.Y2018.Day08Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y1808

  alias AdventOfCode.Y2018.Day08, as: Solution

  test "Year 2018, Day 8" do
    assert Solution.run() == {40_701, nil}
  end
end
