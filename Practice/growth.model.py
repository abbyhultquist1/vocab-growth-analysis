import threading
import random
import tkinter as tkinter
from tkinter import filedialog as tkfiledialog

ENTRY_WIDTH = 20 # width of entry boxes in characters


def browse_for_entry(entry_box, dialog_type, filetype):
    if dialog_type == "open":
        name = tkfiledialog.askopenfilename(filetypes=[filetype])
    else:
        name = tkfiledialog.asksaveasfilename(filetypes=[filetype])
    entry_box.delete(0, tkinter.END)
    entry_box.insert(0, name)


def show_finished(status_label, run_button):
    run_button.configure(text="Run Again", state=tkinter.NORMAL)
    status_label.configure(text="Done.")


def run_dummy(status_label, run_button):
    # placeholder for now
    status_label.configure(text="Pretend model ran ✔")
    show_finished(status_label, run_button)


def run_growth(child_vocab_path, all_words_path, output_path,
               status_label, run_button,
               num_runs=1, num_desired_words=100):

    try:
        # ---------- LOAD ALL WORD CONNECTIONS ----------
        with open(all_words_path, "r") as f:
            combinations = [line.strip().split(",") for line in f]

        # Build connection map
        connection_map = {}
        all_possible_words = set()
        print(all_possible_words)



        for w1, w2 in combinations:
            connection_map.setdefault(w1, set()).add(w2)
            connection_map.setdefault(w2, set()).add(w1)
            all_possible_words.add(w1)
            all_possible_words.add(w2)

        # ---------- LOAD CHILD VOCAB ----------
        with open(child_vocab_path, "r") as f:
            datalist = [tuple(line.strip().split(",")) for line in f]

        # ---------- GROWTH SIMULATION ----------
        summary = {}
        k = 0

        while k < num_runs:
            status_label.configure(text=f"Run {k+1} / {num_runs}")
            run_button.update_idletasks()

            # make a *copy* of child vocab for this run
            current_edges = list(datalist)
            current_words = set(w for edge in current_edges for w in edge)

            added = 0
            # choose new words uniformly from all possible words (not based on neighbor links)
            while added < num_desired_words:
                remaining = list(all_possible_words - current_words)
                if not remaining:
                    # no more words to add
                    break

                new_word = random.choice(remaining)

                # add new word by inheriting edges (if any exist in the connection map)
                for w in current_words:
                    if w in connection_map.get(new_word, set()):
                        current_edges.append((new_word, w))
                        current_edges.append((w, new_word))

                current_words.add(new_word)
                summary[new_word] = summary.get(new_word, 0) + 1
                added += 1

            k += 1

        # ---------- WRITE SUMMARY ----------
        with open(output_path, "w") as f:
            for word, count in sorted(summary.items(),
                                      key=lambda x: x[1],
                                      reverse=True):
                f.write(f"{word},{count}\n")

        status_label.configure(text="Done.")
        run_button.configure(text="Run Again", state=tkinter.NORMAL)

    except Exception as e:
        status_label.configure(text=f"ERROR: {e}")
        run_button.configure(text="Run Growth", state=tkinter.NORMAL)

def run_growth_thread(child_vocab_entry, all_words_entry, output_entry,
                      status_label, run_button):

    run_button.configure(text="Running…", state=tkinter.DISABLED)

    thread = threading.Thread(
        target=run_growth,
        args=(
            child_vocab_entry.get(),
            all_words_entry.get(),
            output_entry.get(),
            status_label,
            run_button
        ),
        daemon=True
    )
    thread.start()


if __name__ == "__main__":
    master = tkinter.Tk()
    master.title("Growth Model Runner")

    tkinter.Label(master, text="Child vocabulary:").grid(row=0, column=0, sticky=tkinter.E)
    tkinter.Label(master, text="All words:").grid(row=1, column=0, sticky=tkinter.E)
    tkinter.Label(master, text="Output summary:").grid(row=2, column=0, sticky=tkinter.E)

    child_vocab_entry = tkinter.Entry(master, width=40)
    child_vocab_entry.grid(row=0, column=1)

    all_words_entry = tkinter.Entry(master, width=40)
    all_words_entry.grid(row=1, column=1)

    output_entry = tkinter.Entry(master, width=40)
    output_entry.grid(row=2, column=1)

    tkinter.Button(
        master, text="Browse",
        command=lambda: browse_for_entry(child_vocab_entry, "open", ("Text files", ".txt"))
    ).grid(row=0, column=2)

    tkinter.Button(
        master, text="Browse",
        command=lambda: browse_for_entry(all_words_entry, "open", ("Text files", ".txt"))
    ).grid(row=1, column=2)

    tkinter.Button(
        master, text="Browse",
        command=lambda: browse_for_entry(output_entry, "saveas", ("CSV files", ".csv"))
    ).grid(row=2, column=2)

    status_label = tkinter.Label(master, text="Ready", width=ENTRY_WIDTH)
    status_label.grid(row=4, column=0, columnspan=3)

    # Single Run button (starts the threaded runner) and make it visible
    run_button = tkinter.Button(
        master,
        text="Run Growth",
        command=lambda: run_growth_thread(
            child_vocab_entry,
            all_words_entry,
            output_entry,
            status_label,
            run_button
        )
    )
    run_button.grid(row=3, column=1)

master.mainloop()



