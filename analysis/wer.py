import pandas as pd 
from evaluate import load

# load WER from Evaluate and read in utterances file
wer = load("wer")
csv_path = "validate_transcripts.csv"
df = pd.read_csv(csv_path)

# utterances to list of strings
predictions = df['text'].astype(str).tolist()
references = df['manual_transcript'].astype(str).tolist()

# computer WER
wer_score = wer.compute(predictions=predictions, references=references)
print(wer_score)
