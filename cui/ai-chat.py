import sys
import argparse
import json
import os
import google.generativeai as genai
from google.generativeai.types import content_types

# --- Helper Functions for History ---

def save_history(filepath: str, history: list[content_types.StrictContentType]):
    """チャット履歴をJSONファイルに保存する"""
    try:
        # geminiのContentオブジェクトをJSONシリアライズ可能な辞書に変換
        serializable_history = []
        for content in history:
            # safety_ratingsやcitation_metadataを除外してシンプルな構造にする
            serializable_parts = [{'text': part.text} for part in content.parts]
            serializable_history.append({
                'role': content.role,
                'parts': serializable_parts
            })
        
        with open(filepath, 'w', encoding='utf-8') as f:
            json.dump(serializable_history, f, ensure_ascii=False, indent=2)
        print(f"\nチャット履歴を {filepath} に保存しました。")
    except Exception as e:
        print(f"\nエラー: 履歴の保存に失敗しました。詳細: {e}")

def load_history(filepath: str) -> list[content_types.StrictContentType]:
    """JSONファイルからチャット履歴を読み込む"""
    if not os.path.exists(filepath):
        return [] # ファイルがなければ空の履歴を返す
    
    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            # ファイルが空の場合の対策
            content = f.read()
            if not content:
                return []
            serializable_history = json.loads(content)
        
        # 辞書をgeminiのContentオブジェクトに変換
        history = [
            content_types.to_content(item) for item in serializable_history
        ]
        print(f"チャット履歴を {filepath} から復元しました。")
        return history
    except (json.JSONDecodeError, FileNotFoundError) as e:
        print(f"警告: 履歴ファイルの読み込みに失敗しました。新しいセッションを開始します。詳細: {e}")
        return []
    except Exception as e:
        print(f"警告: 予期せぬエラーで履歴の読み込みに失敗しました。詳細: {e}")
        return []

# --- Main Application ---

def main():
    """メインの処理を実行する関数"""
    parser = argparse.ArgumentParser(
        description="Gemini APIと対話するCLIツール（永続的な履歴保持機能付き）。",
        formatter_class=argparse.RawTextHelpFormatter
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
    # ★★★ 変更点1: 履歴ファイルを指定する引数を追加 ★★★
    parser.add_argument(
        "--history-file", "-f",
        type=str,
        default=None,
        help="チャット履歴を保存/復元するJSONファイルのパス (例: chat_history.json)"
    )
    args = parser.parse_args()

    # Gemini API の設定
    try:
        genai.configure(api_key=args.api_key)
        model = genai.GenerativeModel(args.model)
        
        # ★★★ 変更点2: ファイルから履歴を読み込む ★★★
        initial_history = []
        if args.history_file:
            initial_history = load_history(args.history_file)
            
        chat = model.start_chat(history=initial_history)
        
    except Exception as e:
        print(f"エラー: APIキーまたはモデル名の設定に失敗しました。詳細: {e}")
        sys.exit(1)

    print("Geminiとの対話を開始します。（永続履歴モード）")
    if args.history_file:
        print(f"履歴ファイル: {args.history_file}")
    print("プロンプトを入力し、最後に '---' だけの行を入力して送信してください。")
    print("プロンプトを何も入力せずに '---' を入力すると終了します。")
    print("-" * 20)

    try:
        while True:
            lines = []
            while True:
                try:
                    line = sys.stdin.readline()
                    if not line or line.strip() == '---':
                        break
                    lines.append(line)
                except UnicodeDecodeError as e:
                    print(f"\n警告: 文字コードエラーを検出しました。入力を無視します。詳細: {e}")
                    # 不正な入力行をクリア
                    lines.clear()
                    break

            prompt = "".join(lines).strip()

            if not prompt:
                # Ctrl+D (EOF) または入力なしでの終了
                break 

            print("\n... Geminiに送信中 ...\n")

            try:
                response = chat.send_message(prompt)
                print(response.text)
                print("===")
            except Exception as e:
                print(f"APIの呼び出し中にエラーが発生しました: {e}")
                print("===")
                
    except KeyboardInterrupt:
        # Ctrl+Cで中断
        print("\nプログラムを中断します。")
    finally:
        # ★★★ 変更点3: プログラム終了時に履歴を保存する ★★★
        if args.history_file and 'chat' in locals() and chat.history:
            save_history(args.history_file, chat.history)
        
        print("プログラムを終了します。")


if __name__ == "__main__":
    main()