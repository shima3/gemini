import os
import sys
import google.generativeai as genai

def main():
    """
    メインの処理を実行する関数
    """
    # 1. 環境変数から Gemini のモデル名と API キーを取得する
    try:
        api_key = os.environ['GEMINI_API_KEY']
        model_name = os.environ['GEMINI_MODEL_NAME']
    except KeyError as e:
        print(f"エラー: 環境変数 {e} が設定されていません。")
        print("実行前に GEMINI_API_KEY と GEMINI_MODEL_NAME を設定してください。")
        sys.exit(1)

    # Gemini API の設定
    genai.configure(api_key=api_key)
    model = genai.GenerativeModel(model_name)

    print("Geminiとの対話を開始します。")
    print("プロンプトを入力し、最後に '---' だけの行を入力して送信してください。")
    print("プロンプトを何も入力せずに '---' を入力すると終了します。")
    print("-" * 20)

    # 6. 2に戻る (無限ループ)
    while True:
        # 2. 標準入力から文字列を入力する
        lines = []
        while True:
            try:
                line = sys.stdin.readline()
                # readline()はEOFで空文字列を返すためチェック
                if not line or line.strip() == '---':
                    break
                lines.append(line)
            except KeyboardInterrupt:
                print("\nプログラムを終了します。")
                sys.exit(0)
        
        prompt = "".join(lines)

        # 3. 1行も入力がなければプログラムを終了する
        if not prompt.strip():
            print("入力がなかったため、プログラムを終了します。")
            break

        print("\n... Geminiに送信中 ...\n")

        try:
            # 4. 入力した文字列を Gemini API で送信する
            response = model.generate_content(prompt)
            
            # 5. Geminiの回答を標準出力に出力した後、イコール3文字だけの行を出力する
            print(response.text)
            print("===")

        except Exception as e:
            print(f"APIの呼び出し中にエラーが発生しました: {e}")
            print("===")

if __name__ == "__main__":
    main()