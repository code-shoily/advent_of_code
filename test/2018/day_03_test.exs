defmodule AdventOfCode.Y2018.Day03Test do
  @moduledoc false
  use ExUnit.Case
  @moduletag :y1803

  alias AdventOfCode.Y2018.Day03, as: Solution

  test "Year 2018, Day 3" do
    assert Solution.run() == {110_389, 552}
  end
end
