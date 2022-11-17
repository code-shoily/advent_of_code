defmodule AdventOfCode.Y2017.Day03Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y1703

  alias AdventOfCode.Y2017.Day03, as: Solution

  test "Year 2017, Day 3 run/1" do
    assert Solution.run() == {430, 312_453}
  end
end
