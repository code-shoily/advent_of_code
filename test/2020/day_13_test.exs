defmodule AdventOfCode.Y2020.Day13Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2013

  alias AdventOfCode.Y2020.Day13, as: Solution

  test "Year 2020, Day 13" do
    assert Solution.run() == {4782, 1_118_684_865_113_056}
  end
end
