defmodule AdventOfCode.Y2016.Day11Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y1611

  alias AdventOfCode.Y2016.Day11, as: Solution

  test "Year 2016, Day 11 run/1" do
    assert Solution.run() == {37, 61}
  end
end
