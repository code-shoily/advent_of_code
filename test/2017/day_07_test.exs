defmodule AdventOfCode.Y2017.Day07Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y1707

  alias AdventOfCode.Y2017.Day07, as: Solution

  test "Year 2017, Day 7 run/1" do
    assert Solution.run() == {"hmvwl", {:todo, 2}}
  end
end
