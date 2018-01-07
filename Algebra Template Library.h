#pragma once

#include<functional>
#include<sstream>
#include<vector>

namespace atl {
	namespace mat {
		const std::string excep_generic = "_ATL_Matrix";
		const std::string excep_out_of_range = excep_generic + "_out_of_range";
		const std::string excep_conflict_size = excep_generic + "_size_conflict";

		inline size_t encode_row_major(size_t row, size_t col, size_t num_col) {
			return (row * num_col) + col;
		}
		inline size_t encode_col_major(size_t row, size_t col, size_t num_row) {
			return (col * num_row) + row;
		}

		template<typename T> class matrix {
		public:
			matrix(const size_t rows = 0, const size_t cols = 0, const T value = T()) {
				resize(rows, cols, value);
			}
			matrix(const size_t rows, const size_t cols, const std::vector<T> val) {
				

			}
			~matrix() {
			}
		private:
			inline T& unsafe_get(size_t row, size_t col) {
				return m_elements[encode_row_major(row, col, m_num_cols)];
			}
			inline const T& unsafe_get(size_t row, size_t col) const {
				return m_elements[encode_row_major(row, col, m_num_cols)];
			}
		public:
			T& operator()(size_t row, size_t col) {
				if (row < m_num_rows && col < m_num_cols) {
					return unsafe_get(row, col);
				}
				throw std::logic_error(excep_out_of_range);
			}
			const T& operator()(size_t row, size_t col) const {
				if (row < m_num_rows && col < m_num_cols) {
					return unsafe_get(row, col);
				}
				throw std::logic_error(excep_out_of_range);
			}

			size_t get_num_rows() const {
				return m_num_rows;
			}
			size_t get_num_cols() const {
				return m_num_cols;
			}

			matrix<T> get_row(size_t row) const {
				if (row < m_num_rows) {
					matrix<T> out_vec(1, m_num_cols);

					for (size_t i = 0; i < m_num_cols; i++) {
						out_vec.unsafe_get(0, i) = unsafe_get(row, i);
					}

					return out_vec;
				}
				else {
					throw std::logic_error(excep_out_of_range);
				}
			}
			matrix<T> get_col(size_t col) const {
				if (col < m_num_cols) {
					matrix<T> out_vec(m_num_rows, 1);

					for (size_t i = 0; i < m_num_rows; i++) {
						out_vec.unsafe_get(i, 0) = unsafe_get(i, col);
					}

					return out_vec;
				}
				else {
					throw std::logic_error(excep_out_of_range);
				}
			}

			void set_row(matrix<T> row_vec, size_t row) {
				if (row < m_num_rows) {
					if (row_vec.get_num_rows() == 1 && row_vec.get_num_cols() == m_num_cols) {
						for (size_t i = 0; i < m_num_cols; ++i) {
							unsafe_get(row, i) = row_vec.unsafe_get(0, i);
						}
					}
					else {
						throw std::logic_error(excep_conflict_size);
					}
				}
				else {
					throw std::logic_error(excep_out_of_range);
				}
			}
			void set_col(matrix<T> col_vec, size_t col) {
				if (col < m_num_cols) {
					if (col_vec.get_num_rows() == m_num_rows && col_vec.get_num_cols() == 1) {
						for (size_t i = 0; i < m_num_rows; ++i) {
							unsafe_get(i, col) = col_vec.unsafe_get(i, 0);
						}
					}
					else {
						throw std::logic_error(excep_conflict_size);
					}
				}
				else {
					throw std::logic_error(excep_out_of_range);
				}
			}

			void set(const T val) {
				for (size_t i = 0; i < m_num_rows * m_num_cols; ++i) {
					m_elements[i] = val;
				}
			}
			void set_row_maj(const size_t rows, const size_t cols, const std::vector<T> val) {
				if (val.size() == rows * cols) {
					for (size_t i = 0; i < row; ++i) {
						for (size_t j = 0; j < cols; ++j) {
							unsafe_get(i, j) = val[encode_row_major(i, j)];
						}
					}
				}
				else {
					throw std::logic_error(excep_conflict_size);
				}
			}
			void set_row_col(const size_t rows, const size_t cols, const std::vector<T> val) {
				if (val.size() == rows * cols) {
					for (size_t i = 0; i < rows; ++i) {
						for (size_t j = 0; j < cols; ++j) {
							unsafe_get(i, j) = val[encode_col_major(i, j)];
						}
					}
				}
				else {
					throw std::logic_error(excep_conflict_size);
				}
			}

			void set(std::function<T(size_t)> func) {
				for (size_t i = 0; i < m_num_rows * m_num_cols; i++) {
					m_elements[i] = func(i);
				}
			}
			void set(std::function<T(size_t, size_t)> func) {
				for (size_t i = 0; i < m_num_rows; i++) {
					for (size_t j = 0; j < m_num_cols; j++) {
						unsafe_get(i, j) = func(i, j);
					}
				}
			}

			matrix<T>& operator+=(const matrix<T> &mat) {
				if (m_num_rows == mat.m_num_rows && m_num_cols == mat.m_num_cols) {
					for (size_t i = 0; i < m_num_rows * m_num_cols; ++i) {
						m_elements[i] += mat.m_elements[i];
					}
					return *this;
				}
				else {
					throw std::logic_error(excep_conflict_size);
				}
			}
			matrix<T>& operator-=(const matrix<T> &mat) {
				if (m_num_rows == mat.m_num_rows && m_num_cols == mat.m_num_cols) {
					for (size_t i = 0; i < m_num_rows * m_num_cols; ++i) {
						m_elements[i] -= mat.m_elements[i];
					}
					return *this;
				}
				else {
					throw std::logic_error(excep_conflict_size);
				}
			}
			matrix<T>& operator*=(const matrix<T> &mat) {
				if (m_num_cols == mat.m_num_rows) {
					size_t col = mat.m_num_cols;
					std::vector<T> cache(m_num_rows * col);
					T sum;

					for (size_t i = 0; i < m_num_rows; ++i) {
						for (size_t j = 0; j < col; ++j) {
							for (size_t k = 0; k < m_num_cols; ++k) {
								cache[encode_row_major(i, j, col)] += (unsafe_get(i, k) * mat.unsafe_get(k, j));
							}
						}
					}

					m_elements = cache;
					m_num_rows = m_num_rows;
					m_num_cols = col;

					return *this;
				}
				else {
					throw std::logic_error(excep_conflict_size);
				}
			}
			matrix<T>& operator*=(const T &value) {
				for (size_t i = 0; i < m_num_rows * m_num_cols; ++i) {
					m_elements[i] *= value;
				}
				return *this;
			}

			matrix<T> transpose() const {
				matrix<T> out_mat(m_num_cols, m_num_rows);
				for (size_t i = 0; i < m_num_rows; i++) {
					for (size_t j = 0; j < m_num_cols; j++) {
						out_mat.unsafe_get(j, i) = unsafe_get(i, j);
					}
				}

				return out_mat;
			}

			std::string str(const std::string sep = ",", const std::string begin = "[", const std::string end = "]\n") const {
				std::stringstream out;
				for (size_t i = 0; i < m_num_rows; i++) {
					out << begin;
					for (size_t j = 0; j < m_num_cols - 1; j++) {
						out << unsafe_get(i, j) << sep;
					}
					out << unsafe_get(i, m_num_cols - 1) << end;
				}

				return out.str();
			}
			void resize(const size_t rows, const size_t cols, const T &value = T()) {
				std::vector<T> temp_elements(rows * cols);
				size_t min_rows, min_cols;
				min_rows = (rows < m_num_rows) ? rows : m_num_cols;
				min_cols = (rows < m_num_cols) ? rows : m_num_cols;

				for (size_t i = 0; i < min_rows; ++i) {
					for (size_t j = 0; j < min_cols; ++j) {
						temp_elements[encode_row_major(i, j, min_cols)] = unsafe_get(i, j);
					}
					for (size_t j = min_cols; j < cols; ++j) {
						temp_elements[encode_row_major(i, j, min_cols)] = T();
					}
				}
				for (size_t i = min_rows; i < rows; ++i) {
					for (size_t j = 0; j < cols; ++j) {
						temp_elements[encode_row_major(i, j, min_cols)] = T();
					}
				}
				
				m_elements = temp_elements;
				
				m_num_rows = rows;
				m_num_cols = cols;
			}
		private:
			size_t m_num_rows;
			size_t m_num_cols;

			std::vector<T> m_elements;
		};

		template<typename T>
		std::ostream& operator<<(std::ostream &out, const matrix<T> &mat) {
			return out << mat.str();
		}
		template<typename T>
		inline matrix<T> operator+(matrix<T> l_mat, const matrix<T> &r_mat) {
			l_mat += r_mat;
			return l_mat;
		}
		template<typename T>
		inline matrix<T> operator-(matrix<T> l_mat, const matrix<T> &r_mat) {
			l_mat -= r_mat;
			return l_mat;
		}
		template<typename T>
		inline matrix<T> operator*(matrix<T> l_mat, const matrix<T> &r_mat) {
			l_mat *= r_mat;
			return l_mat;
		}
		template<typename T>
		inline matrix<T> operator*(matrix<T> l_mat, const T &r_val) {
			l_mat *= r_val;
			return l_mat;
		}
		template<typename T>
		inline matrix<T> operator*(const T &l_val, matrix<T> r_mat) {
			r_mat *= l_val;
			return r_mat;
		}

		template<typename T, size_t R, size_t C> class f_matrix {
		public:
			f_matrix(T value = T()) {
				m_matrix.resize(R, C, value);
			}
			~f_matrix() {
			}
		public:
			T& at(size_t row, size_t col) {
				return m_matrix(row, col);
			}
			const T& at(size_t row, size_t col) const {
				return m_matrix(row, col);
			}

			explicit operator const matrix<T>() const {
				return m_matrix;
			}

			f_matrix<T, 1, C> get_row(size_t row) {
				f_matrix<T, 1, C> out_vec;
				out_vec.m_matrix = m_matrix.get_row(row);

				return out_vec;
			}
			f_matrix<T, R, 1> get_col(size_t col) {
				f_matrix<T, R, 1> out_vec;
				out_vec.m_matrix = m_matrix.get_col(col);

				return out_vec;
			}
			void set_row(const f_matrix<T, 1, C> &row_vec, size_t row) {
				m_matrix.set_row(static_cast<matrix<T>>(row_vec), row);
			}
			void set_col(const f_matrix<T, R, 1> &col_vec, size_t col) {
				m_matrix.set_col(static_cast<matrix<T>>(col_vec), col);
			}

			void set(T val) {
				m_matrix.set(val);
			}
			void set_row_maj(const size_t rows, const size_t cols, const std::vector<T> val){
				m_matrix.set_row_maj(rows, cols, val);
			}
			void set_col_maj(const size_t rows, const size_t cols, const std::vector<T> val) {
				m_matrix.set_col_maj(rows, cols, val);
			}

			f_matrix<T, R, C> operator+=(const f_matrix<T, R, C> &mat) {
				m_matrix += static_cast<matrix<T>>(mat);
				return *this;
			}
			f_matrix<T, R, C> operator-=(const f_matrix<T, R, C> &mat) {
				m_matrix -= static_cast<matrix<T>>(mat);
				return *this;
			}
			f_matrix<T, R, C> operator*=(const f_matrix<T, C, C> &mat) {
				m_matrix *= static_cast<matrix<T>>(mat);
				return *this;
			}
			f_matrix<T, R, C> operator*=(const T &value) {
				m_matrix *= value;
				return *this;
			}
			template<size_t CR>
			static f_matrix<T, R, C> mult(const f_matrix<T, R, CR> &mat_1, const f_matrix<T, CR, C> &mat_2) {
				f_matrix<T, R, C> out_mat;
				out_mat.m_matrix = static_cast<matrix<T>>(mat_1) * static_cast<matrix<T>>(mat_2);

				return out_mat;
			}

			f_matrix<T, C, R> transpose() const {
				f_matrix<T, C, R> out_mat;
				out_mat.m_matrix = m_matrix.transpose();
				return out_mat;
			}

			std::string str(const std::string sep = ",", const std::string begin = "[", const std::string end = "]\n") const {
				return m_matrix.str();
			}
		protected:
		private:
			template<typename T, size_t _R, size_t _C> friend class f_matrix;

			matrix<T> m_matrix;
		};

		template<typename T, size_t R, size_t C>
		std::ostream& operator<<(std::ostream &out, const f_matrix<T, R, C> &mat) {
			return out << mat.str();
		}
		template<typename T, size_t R, size_t C>
		inline f_matrix<T, R, C> operator+(f_matrix<T, R, C> l_mat, const f_matrix<T, R, C> &r_mat) {
			l_mat += r_mat;
			return l_mat;
		}
		template<typename T, size_t R, size_t C>
		inline f_matrix<T, R, C> operator-(f_matrix<T, R, C> l_mat, const f_matrix<T, R, C> &r_mat) {
			l_mat -= r_mat;
			return l_mat;
		}
		template<typename T, size_t R, size_t CR, size_t C>
		inline f_matrix<T, R, C> operator*(const f_matrix<T, R, CR> &l_mat, const f_matrix<T, CR, C> &r_mat) {
			return f_matrix<T, R, C>::mult(l_mat, r_mat);
		}
		template<typename T, size_t R, size_t C>
		inline f_matrix<T, R, C> operator*(f_matrix<T, R, C> l_mat, const T &r_val) {
			l_mat *= r_val;
			return l_mat;
		}
		template<typename T, size_t R, size_t C>
		inline f_matrix<T, R, C> operator*(const T &l_val, f_matrix<T, R, C> r_mat) {
			r_mat *= l_val;
			return r_mat;
		}
	}
}
