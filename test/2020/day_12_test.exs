defmodule AdventOfCode.Y2020.Day12Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2012

  alias AdventOfCode.Y2020.Day12, as: Solution

  test "Year 2020, Day 12" do
    assert Solution.run() == {439, 12_385}
  end
end
