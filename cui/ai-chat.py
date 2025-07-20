import sys
import argparse
import google.generativeai as genai

def main():
    """
    メインの処理を実行する関数
    """
    # 1. コマンド引数から Gemini のモデル名と API キーを取得する
    parser = argparse.ArgumentParser(
        description="Gemini APIと対話するCLIツール。",
        formatter_class=argparse.RawTextHelpFormatter # ヘルプメッセージの改行を保持
    )
    parser.add_argument(
        "--model", "-m", 
        type=str, 
        required=True, 
        help="使用するGeminiのモデル名 (例: gemini-1.5-flash-latest)"
    )
    parser.add_argument(
        "--api-key", "-k", 
        type=str, 
        required=True, 
        help="あなたのGemini APIキー"
    )
    args = parser.parse_args()

    # Gemini API の設定
    try:
        genai.configure(api_key=args.api_key)
        model = genai.GenerativeModel(args.model)
    except Exception as e:
        print(f"エラー: APIキーまたはモデル名の設定に失敗しました。")
        print(f"詳細: {e}")
        sys.exit(1)


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
                # readline()はEOFで空文字列を返すか、入力終了の合図かチェック
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