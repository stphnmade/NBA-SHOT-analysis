import kagglehub
from kagglehub import KaggleDatasetAdapter

file_path = "~/NBA-shot-analysis/NBA-SHOT-analysis/data"

df = kagglehub.load_dataset(
    KaggleDatasetAdapter.PANDAS,
    "mexwell/nba-shots",
    file_path,
)

print("First 5 records:", df.head())

