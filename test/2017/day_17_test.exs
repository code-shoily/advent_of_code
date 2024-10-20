defmodule AdventOfCode.Y2017.Day17Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y1717

  alias AdventOfCode.Y2017.Day17, as: Solution

  test "Year 2017, Day 17 run/1" do
    assert Solution.run() == {926, 10_150_888}
  end
end
