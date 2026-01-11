#include "dsm.hpp"
#include <algorithm>
#include <stdexcept>

namespace dsm {

DSM::DSM(MPI_Comm comm, int num_variables)
    : comm_(comm), num_variables_(num_variables), variables_(num_variables) {
    MPI_Comm_rank(comm_, &rank_);
    MPI_Comm_size(comm_, &size_);
}

DSM::~DSM() {
    if (!closed_) {
        close();
    }
}

void DSM::subscribe(int var_id, const std::set<int>& subscribers) {
    if (var_id < 0 || var_id >= num_variables_) {
        throw std::out_of_range("Invalid variable ID");
    }

    Variable& var = variables_[var_id];
    var.subscribers = subscribers;

    if (!subscribers.empty()) {
        var.coordinator = *subscribers.begin();
    }
}

void DSM::set_callback(ChangeCallback cb) {
    callback_ = std::move(cb);
}

int DSM::read(int var_id) const {
    if (var_id < 0 || var_id >= num_variables_) {
        throw std::out_of_range("Invalid variable ID");
    }
    return variables_[var_id].value;
}

void DSM::write(int var_id, int value) {
    if (!is_subscribed(var_id)) {
        throw std::runtime_error("Not subscribed to variable");
    }

    Variable& var = variables_[var_id];
    int req_id = next_request_id_++;

    if (rank_ == var.coordinator) {
        Message commit;
        commit.type = static_cast<int>(MessageType::WRITE_COMMIT);
        commit.var_id = var_id;
        commit.value = value;
        commit.seq_num = var.next_seq++;
        commit.sender = rank_;
        commit.request_id = req_id;

        broadcast_to_subscribers(var_id, commit);
        apply_commit(var_id, commit);
        completed_requests_.insert(req_id);
    } else {
        Message request;
        request.type = static_cast<int>(MessageType::WRITE_REQUEST);
        request.var_id = var_id;
        request.value = value;
        request.sender = rank_;
        request.request_id = req_id;

        send_message(var.coordinator, request);

        while (completed_requests_.find(req_id) == completed_requests_.end()) {
            process_messages();
        }
    }
}

bool DSM::compare_exchange(int var_id, int expected, int new_value) {
    if (!is_subscribed(var_id)) {
        throw std::runtime_error("Not subscribed to variable");
    }

    Variable& var = variables_[var_id];
    int req_id = next_request_id_++;

    if (rank_ == var.coordinator) {
        if (var.value == expected) {
            Message commit;
            commit.type = static_cast<int>(MessageType::CAS_COMMIT);
            commit.var_id = var_id;
            commit.value = expected;
            commit.new_value = new_value;
            commit.seq_num = var.next_seq++;
            commit.sender = rank_;
            commit.request_id = req_id;

            broadcast_to_subscribers(var_id, commit);
            apply_commit(var_id, commit);
            return true;
        }
        return false;
    } else {
        Message request;
        request.type = static_cast<int>(MessageType::CAS_REQUEST);
        request.var_id = var_id;
        request.value = expected;
        request.new_value = new_value;
        request.sender = rank_;
        request.request_id = req_id;

        pending_cas_result_[req_id] = false;
        cas_result_ready_[req_id] = false;

        send_message(var.coordinator, request);

        while (!cas_result_ready_[req_id]) {
            process_messages();
        }

        return pending_cas_result_[req_id];
    }
}

void DSM::sync() {
    MPI_Request barrier_req;
    MPI_Ibarrier(comm_, &barrier_req);

    int complete = 0;
    while (!complete) {
        process_messages();
        MPI_Test(&barrier_req, &complete, MPI_STATUS_IGNORE);
    }
}

void DSM::close() {
    closed_ = true;
}

void DSM::send_message(int dest, const Message& msg) {
    MPI_Send(&msg, sizeof(Message), MPI_BYTE, dest, 0, comm_);
}

void DSM::broadcast_to_subscribers(int var_id, const Message& msg) {
    const Variable& var = variables_[var_id];
    for (int sub : var.subscribers) {
        if (sub != rank_) {
            send_message(sub, msg);
        }
    }
}

void DSM::process_messages() {
    int flag = 0;
    MPI_Status status;

    while (true) {
        MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, comm_, &flag, &status);
        if (!flag) break;

        Message msg;
        MPI_Recv(&msg, sizeof(Message), MPI_BYTE, status.MPI_SOURCE,
                 status.MPI_TAG, comm_, MPI_STATUS_IGNORE);
        handle_message(msg, status.MPI_SOURCE);
    }
}

void DSM::handle_message(const Message& msg, int source) {
    MessageType type = static_cast<MessageType>(msg.type);

    switch (type) {
        case MessageType::WRITE_REQUEST: {
            Variable& var = variables_[msg.var_id];

            Message commit;
            commit.type = static_cast<int>(MessageType::WRITE_COMMIT);
            commit.var_id = msg.var_id;
            commit.value = msg.value;
            commit.seq_num = var.next_seq++;
            commit.sender = msg.sender;
            commit.request_id = msg.request_id;

            broadcast_to_subscribers(msg.var_id, commit);
            apply_commit(msg.var_id, commit);
            break;
        }

        case MessageType::WRITE_COMMIT: {
            Variable& var = variables_[msg.var_id];

            if (msg.seq_num == var.last_applied_seq + 1) {
                apply_commit(msg.var_id, msg);
                try_apply_pending(msg.var_id);
            } else if (msg.seq_num > var.last_applied_seq + 1) {
                var.pending_commits[msg.seq_num] = msg;
            }

            if (msg.sender == rank_) {
                completed_requests_.insert(msg.request_id);
            }
            break;
        }

        case MessageType::CAS_REQUEST: {
            Variable& var = variables_[msg.var_id];
            bool success = (var.value == msg.value);

            if (success) {
                Message commit;
                commit.type = static_cast<int>(MessageType::CAS_COMMIT);
                commit.var_id = msg.var_id;
                commit.value = msg.value;
                commit.new_value = msg.new_value;
                commit.seq_num = var.next_seq++;
                commit.sender = msg.sender;
                commit.request_id = msg.request_id;

                broadcast_to_subscribers(msg.var_id, commit);
                apply_commit(msg.var_id, commit);
            } else {
                Message response;
                response.type = static_cast<int>(MessageType::CAS_RESPONSE);
                response.var_id = msg.var_id;
                response.value = 0;
                response.sender = rank_;
                response.request_id = msg.request_id;

                send_message(source, response);
            }
            break;
        }

        case MessageType::CAS_COMMIT: {
            Variable& var = variables_[msg.var_id];

            if (msg.seq_num == var.last_applied_seq + 1) {
                apply_commit(msg.var_id, msg);
                try_apply_pending(msg.var_id);
            } else if (msg.seq_num > var.last_applied_seq + 1) {
                var.pending_commits[msg.seq_num] = msg;
            }

            if (msg.sender == rank_) {
                pending_cas_result_[msg.request_id] = true;
                cas_result_ready_[msg.request_id] = true;
            }
            break;
        }

        case MessageType::CAS_RESPONSE: {
            pending_cas_result_[msg.request_id] = false;
            cas_result_ready_[msg.request_id] = true;
            break;
        }
    }
}

void DSM::apply_commit(int var_id, const Message& msg) {
    Variable& var = variables_[var_id];
    int old_value = var.value;

    MessageType type = static_cast<MessageType>(msg.type);

    if (type == MessageType::CAS_COMMIT) {
        if (var.value == msg.value) {
            var.value = msg.new_value;
        }
    } else {
        var.value = msg.value;
    }

    var.last_applied_seq = msg.seq_num;

    if (callback_ && old_value != var.value) {
        callback_(var_id, old_value, var.value);
    }
}

void DSM::try_apply_pending(int var_id) {
    Variable& var = variables_[var_id];

    while (true) {
        auto it = var.pending_commits.find(var.last_applied_seq + 1);
        if (it == var.pending_commits.end()) break;

        apply_commit(var_id, it->second);
        var.pending_commits.erase(it);
    }
}

bool DSM::is_subscribed(int var_id) const {
    if (var_id < 0 || var_id >= num_variables_) return false;
    return variables_[var_id].subscribers.count(rank_) > 0;
}

int DSM::get_coordinator(int var_id) const {
    return variables_[var_id].coordinator;
}

} // namespace dsm
