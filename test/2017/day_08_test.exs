defmodule AdventOfCode.Y2017.Day08Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y1708

  alias AdventOfCode.Y2017.Day08, as: Solution

  test "Year 2017, Day 8 run/1" do
    assert Solution.run() == {2971, 4254}
  end
end
