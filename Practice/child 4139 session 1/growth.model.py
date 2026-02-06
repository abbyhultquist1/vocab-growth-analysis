import threading
import tkinter as tkinter
from tkinter import filedialog as tkfiledialog

ENTRY_WIDTH = 20


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

    run_button = tkinter.Button(
        master, text="Run Growth",
        command=lambda: run_dummy(status_label, run_button)
    )
    run_button.grid(row=3, column=0, columnspan=3, sticky=tkinter.E+tkinter.W)

    master.mainloop()
