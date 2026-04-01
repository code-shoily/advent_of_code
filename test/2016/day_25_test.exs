defmodule AdventOfCode.Y2016.Day25Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y1625

  alias AdventOfCode.Y2016.Day25, as: Solution

  test "Year 2016, Day 25 run/1" do
    assert Solution.run() == {158, :done}
  end
end
