defmodule AdventOfCode.Y2016.Day13Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y1613

  alias AdventOfCode.Y2016.Day13, as: Solution

  test "Year 2016, Day 13 run/1" do
    assert Solution.run() == {92, 124}
  end
end
