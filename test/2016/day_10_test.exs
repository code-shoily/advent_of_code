defmodule AdventOfCode.Y2016.Day10Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y1610

  alias AdventOfCode.Y2016.Day10, as: Solution

  test "Year 2016, Day 10, both parts" do
    assert Solution.run() == {56, 7847}
  end
end
