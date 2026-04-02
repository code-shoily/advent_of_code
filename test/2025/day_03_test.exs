defmodule AdventOfCode.Y2025.Day03Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2503

  alias AdventOfCode.Y2025.Day03, as: Solution

  test "Year 2025, Day 3 run/1" do
    assert Solution.run() == {17343, 172_664_333_119_298}
  end
end
