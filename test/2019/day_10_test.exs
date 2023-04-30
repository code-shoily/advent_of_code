defmodule AdventOfCode.Y2019.Day10Test do
  @moduledoc false
  use ExUnit.Case
  @moduletag :y1910

  alias AdventOfCode.Y2019.Day10, as: Solution

  test "Year 2019, Day 10" do
    assert Solution.run() == {263, 1110}
  end
end
