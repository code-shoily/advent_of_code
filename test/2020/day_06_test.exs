defmodule AdventOfCode.Y2020.Day06Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2006

  alias AdventOfCode.Y2020.Day06, as: Solution

  test "Year 2020, Day 6" do
    assert Solution.run() == {6885, 3550}
  end
end
