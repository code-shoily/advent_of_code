defmodule AdventOfCode.Y2017.Day13Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y1713

  alias AdventOfCode.Y2017.Day13, as: Solution

  test "Year 2017, Day 13 run/1" do
    assert Solution.run() == {2604, 3_941_460}
  end
end
