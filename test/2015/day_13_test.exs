defmodule AdventOfCode.Y2015.Day13Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y1513

  alias AdventOfCode.Y2015.Day13, as: Solution

  test "Year 2015, Day 13, Both" do
    assert Solution.run() == {733, 725}
  end
end
