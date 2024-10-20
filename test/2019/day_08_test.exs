defmodule AdventOfCode.Y2019.Day08Test do
  @moduledoc false
  use ExUnit.Case, async: true
  @moduletag :y1908

  alias AdventOfCode.Y2019.Day08, as: Solution

  test "Year 2019, Day 8" do
    assert Solution.run() == {1572, :ok}
  end
end
