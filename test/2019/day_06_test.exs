defmodule AdventOfCode.Y2019.Day06Test do
  @moduledoc false
  use ExUnit.Case, async: true
  @moduletag :y1906

  alias AdventOfCode.Y2019.Day06, as: Solution

  test "Year 2019, Day 6" do
    assert Solution.run() == {147_807, 229}
  end
end
