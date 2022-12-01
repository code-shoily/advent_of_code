defmodule AdventOfCode.Y2016.Day12Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y1612

  alias AdventOfCode.Y2016.Day12, as: Solution

  test "Year 2016, Day 12" do
    assert Solution.run() == {318_077, 9_227_731}
  end
end
