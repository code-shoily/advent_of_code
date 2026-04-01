defmodule AdventOfCode.Y2021.Day17Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2117

  alias AdventOfCode.Y2021.Day17, as: Solution

  test "Year 2021, Day 17 run/1" do
    assert Solution.run() == {6786, 2313}
  end
end
