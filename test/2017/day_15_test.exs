defmodule AdventOfCode.Y2017.Day15Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y1715

  alias AdventOfCode.Y2017.Day15, as: Solution

  @tag :skip_slow
  test "Year 2017, Day 15 run/1" do
    assert Solution.run() == {631, 279}
  end
end
